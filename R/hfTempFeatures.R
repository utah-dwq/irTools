#' Build daily dry-sensor features for HF water temperature
#'
#' Aggregates high-frequency water temperature to one row per Sample_ID and Date with the continuous variability, air-water, rolling, and harmonic physics features used by the dry-sensor Random Forest. This is the single feature definition shared by model training (highfreqQC) and IR scoring (\code{flagDryHFTemp}); changing it requires bumping \code{hfTempFeatureVersion()} and retraining.
#'
#' @param hf_data data.frame with columns \code{DateTime_Local}, \code{Sample_ID}, \code{HF_Location_ID}, \code{Water_Measurement}, \code{tmmn_mean} (daily min air temp, C), and \code{tmmx_mean} (daily max air temp, C).
#' @param air_max_hour Local hour assumed for the daily air temperature maximum when computing phase lag. Default 15.
#' @return data.table with one row per Sample_ID and Date.
#' @export buildHFTempFeatures
buildHFTempFeatures <- function(hf_data, air_max_hour = 15) {
  if (!data.table::is.data.table(hf_data)) {
    hf_data <- data.table::as.data.table(hf_data)
  }

  message("Step 1: Calculating mode sampling interval...")
  mode_intervals <- hf_data %>%
    dplyr::filter(!is.na(DateTime_Local)) %>%
    dplyr::group_by(Sample_ID, HF_Location_ID) %>%
    dplyr::arrange(DateTime_Local, .by_group = TRUE) %>%
    dplyr::mutate(
      time_diff = as.numeric(
        difftime(DateTime_Local, dplyr::lag(DateTime_Local), units = "mins")
      )
    ) %>%
    dplyr::filter(!is.na(time_diff)) %>%
    dplyr::count(time_diff, name = "counts") %>%
    dplyr::slice_max(order_by = counts, n = 1, with_ties = FALSE) %>%
    dplyr::select(
      Sample_ID, HF_Location_ID,
      Mode_Time_Difference_Minutes = time_diff
    ) %>%
    dplyr::ungroup()

  if (any(duplicated(mode_intervals$Sample_ID))) {
    warning("Some samples have multiple mode intervals; keeping only the most frequent.")
    mode_intervals <- mode_intervals %>%
      dplyr::group_by(Sample_ID) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  message("Step 2: Preparing high-frequency data and calculating rolling features...")
  all_temp <- merge(
    hf_data, data.table::as.data.table(mode_intervals),
    by = c("Sample_ID", "HF_Location_ID"),
    all.x = TRUE
  )
  data.table::setkey(all_temp, Sample_ID, DateTime_Local)

  # frollapply needs a positive finite window; irregular deployments can have 0/NA/Inf modes.
  all_temp[
    is.na(Mode_Time_Difference_Minutes) |
      !is.finite(Mode_Time_Difference_Minutes) |
      Mode_Time_Difference_Minutes <= 0,
    Mode_Time_Difference_Minutes := NA_real_
  ]
  all_temp[, n_obs_2hour := ceiling(120 / Mode_Time_Difference_Minutes)]

  all_temp[, rolling_sd_2h := {
    n <- data.table::first(n_obs_2hour)
    n_int <- suppressWarnings(as.integer(n))
    if (is.na(n) || !is.finite(n) || is.na(n_int) || n_int < 2L) {
      rep(NA_real_, .N)
    } else {
      data.table::frollapply(
        Water_Measurement,
        N = n_int,
        FUN = stats::sd,
        align = "right",
        fill = NA
      )
    }
  }, by = Sample_ID]

  all_temp[, Date := data.table::as.IDate(DateTime_Local)]

  message("Step 3: Aggregating to create daily features...")
  daily_feat <- all_temp[!is.na(Water_Measurement), .(
    water_temp_min = min(Water_Measurement, na.rm = TRUE),
    water_temp_max = max(Water_Measurement, na.rm = TRUE),
    water_temp_mean = mean(Water_Measurement, na.rm = TRUE),
    range_d = max(Water_Measurement, na.rm = TRUE) - min(Water_Measurement, na.rm = TRUE),
    sd_d = stats::sd(Water_Measurement, na.rm = TRUE),
    n_rec = .N,
    air_temp_min = data.table::first(tmmn_mean),
    air_temp_max = data.table::first(tmmx_mean),
    sd_deriv_d = stats::sd(diff(Water_Measurement), na.rm = TRUE),
    mean_abs_diff = mean(abs(diff(Water_Measurement)), na.rm = TRUE),
    max_consec_jump = if (length(Water_Measurement) > 1) {
      max(abs(diff(Water_Measurement)), na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_rolling_sd_2h = mean(rolling_sd_2h, na.rm = TRUE),
    max_rolling_sd_2h = if (all(is.na(rolling_sd_2h))) {
      NA_real_
    } else {
      max(rolling_sd_2h, na.rm = TRUE)
    },
    sampling_freq_min = data.table::first(Mode_Time_Difference_Minutes)
  ), by = .(Sample_ID, Date)]

  daily_feat[is.infinite(max_consec_jump), max_consec_jump := NA]

  message("Step 3b: Amplitude ratio + phase lag vs air...")
  phys <- hfTempPhysicsFeatures(all_temp, air_max_hour = air_max_hour)
  daily_feat <- merge(daily_feat, phys, by = c("Sample_ID", "Date"), all.x = TRUE)

  message("Step 4: Calculating continuous air-water metrics...")
  daily_feat[, `:=`(
    diff_min = abs(water_temp_min - air_temp_min),
    diff_max = abs(water_temp_max - air_temp_max),
    diff_mean = abs(water_temp_mean - ((air_temp_min + air_temp_max) / 2)),
    air_temp_range = air_temp_max - air_temp_min,
    avg_diff = (abs(water_temp_min - air_temp_min) + abs(water_temp_max - air_temp_max)) / 2
  )]

  daily_feat[, range_ratio := ifelse(air_temp_range > 0, range_d / air_temp_range, NA)]
  daily_feat[, cv_diff := ifelse(mean_abs_diff > 0, sd_deriv_d / mean_abs_diff, NA)]

  message("Step 5: Calculating multi-day rolling features and z-scores...")
  data.table::setkey(daily_feat, Sample_ID, Date)

  daily_feat[, `:=`(
    movavg_3d = data.table::frollmean(water_temp_max, 3, align = "right", fill = NA),
    movavg_7d = data.table::frollmean(water_temp_max, 7, align = "right", fill = NA),
    mad_7d = data.table::frollapply(water_temp_max, 7, stats::mad, align = "right", fill = NA),
    month = data.table::month(Date)
  ), by = Sample_ID]

  daily_feat[, `:=`(
    zscore_global = (water_temp_max - mean(water_temp_max, na.rm = TRUE)) /
      (stats::sd(water_temp_max, na.rm = TRUE) + 0.001),
    mod_zscore_global = 0.6745 *
      (water_temp_max - stats::median(water_temp_max, na.rm = TRUE)) /
      (stats::mad(water_temp_max, na.rm = TRUE) + 0.001),
    mod_zscore_7d = (water_temp_max -
      data.table::frollapply(water_temp_max, 7, stats::median, align = "right", fill = NA)) /
      (1.4826 * data.table::frollapply(water_temp_max, 7, stats::mad, align = "right", fill = NA) +
        0.001)
  ), by = Sample_ID]

  message("Feature engineering complete.")
  daily_feat
}

#' Daily harmonic amplitude ratio and phase lag versus air temperature
#'
#' Fits a 24-hour harmonic to each day of HF water temperature and compares it to a sinusoid between the daily air min and max (peaking at \code{air_max_hour}). Air-exposed sensors have amplitude ratios near 1 and phase lags near 0.
#'
#' @param hf_data data.frame with \code{DateTime_Local}, \code{Sample_ID}, \code{Water_Measurement}, \code{tmmn_mean}, \code{tmmx_mean}.
#' @param air_max_hour Local hour of the assumed daily air maximum. Default 15.
#' @return data.table keyed by Sample_ID and Date with \code{amplitude_ratio}, \code{phase_lag_hours}, and supporting columns.
#' @export hfTempPhysicsFeatures
hfTempPhysicsFeatures <- function(hf_data, air_max_hour = 15) {
  x <- data.table::copy(data.table::as.data.table(hf_data))
  x[, Date := data.table::as.IDate(DateTime_Local)]
  lt <- as.POSIXlt(x$DateTime_Local)
  x[, hour_local := lt$hour + lt$min / 60 + lt$sec / 3600]

  x[!is.na(Water_Measurement), {
    harm <- .hfFitDayHarmonic(hour_local, Water_Measurement)
    obs_amp <- {
      rng <- range(Water_Measurement, na.rm = TRUE)
      if (any(!is.finite(rng))) NA_real_ else (rng[2] - rng[1]) / 2
    }
    ih <- which.max(Water_Measurement)
    hour_obs_max <- if (length(ih) && is.finite(ih)) hour_local[ih][1] else NA_real_
    air_min <- data.table::first(tmmn_mean)
    air_max <- data.table::first(tmmx_mean)
    air_amp <- if (is.finite(air_min) && is.finite(air_max)) {
      (air_max - air_min) / 2
    } else {
      NA_real_
    }
    water_amp <- if (is.finite(harm$amp)) harm$amp else obs_amp
    water_phase <- if (is.finite(harm$phase_hour)) harm$phase_hour else hour_obs_max
    .(
      water_amp_harm = harm$amp,
      water_amp_obs = obs_amp,
      water_phase_hour = water_phase,
      hour_of_max_obs = hour_obs_max,
      harmonic_r2 = harm$r2,
      air_amp_harm = air_amp,
      amplitude_ratio = if (is.finite(water_amp) && is.finite(air_amp) && air_amp > 0) {
        water_amp / air_amp
      } else {
        NA_real_
      },
      phase_lag_hours = if (is.finite(water_phase)) {
        .hfWrapHourLag(water_phase - air_max_hour)
      } else {
        NA_real_
      }
    )
  }, by = .(Sample_ID, Date)]
}

#' Dry-sensor feature contract
#'
#' \code{hfTempFeatureVersion()} identifies the feature definition in \code{buildHFTempFeatures()}. Saved models record the version they were trained with, and \code{loadDryModel()} refuses models built on a different version. \code{hfTempPredictorCols()} lists the Random Forest predictors; \code{hfTempPhysicsCols()} lists physics evidence kept outside the Random Forest so physics-selected labels are not circular.
#'
#' @return Character vector.
#' @export hfTempFeatureVersion
hfTempFeatureVersion <- function() {
  "dry_features_v3_physics"
}

#' @rdname hfTempFeatureVersion
#' @export hfTempPredictorCols
hfTempPredictorCols <- function() {
  c(
    "zscore_global", "range_d", "sd_d", "sd_deriv_d",
    "mean_abs_diff", "max_consec_jump", "cv_diff",
    "mean_rolling_sd_2h", "max_rolling_sd_2h",
    "movavg_3d", "movavg_7d", "mad_7d",
    "mod_zscore_global", "mod_zscore_7d",
    "diff_min", "diff_max", "avg_diff",
    "range_ratio", "air_temp_range"
  )
}

#' @rdname hfTempFeatureVersion
#' @export hfTempPhysicsCols
hfTempPhysicsCols <- function() {
  c("amplitude_ratio", "phase_lag_hours")
}

.hfWrapHourLag <- function(x) {
  ((as.numeric(x) + 12) %% 24) - 12
}

.hfHarmonicPhaseHour <- function(a_sin, b_cos) {
  # a sin(wt) + b cos(wt) = R sin(wt + phi) peaks at wt + phi = pi/2
  phi <- atan2(b_cos, a_sin)
  as.numeric((6 - (phi * 12 / pi)) %% 24)
}

.hfFitDayHarmonic <- function(hour_local, y, min_n = 8L) {
  ok <- is.finite(hour_local) & is.finite(y)
  if (sum(ok) < min_n) {
    return(list(amp = NA_real_, phase_hour = NA_real_, r2 = NA_real_))
  }
  h <- hour_local[ok]
  yy <- y[ok]
  w <- 2 * pi / 24
  X <- cbind(1, sin(w * h), cos(w * h))
  coef <- tryCatch(qr.solve(X, yy), error = function(e) NULL)
  if (is.null(coef) || length(coef) < 3 || any(!is.finite(coef))) {
    return(list(amp = NA_real_, phase_hour = NA_real_, r2 = NA_real_))
  }
  a <- coef[2]
  b <- coef[3]
  yhat <- as.numeric(X %*% coef)
  ss_res <- sum((yy - yhat)^2)
  ss_tot <- sum((yy - mean(yy))^2)
  list(
    amp = as.numeric(sqrt(a * a + b * b)),
    phase_hour = .hfHarmonicPhaseHour(a, b),
    r2 = if (ss_tot <= 0) NA_real_ else as.numeric(1 - ss_res / ss_tot)
  )
}

.datatable.aware <- TRUE
