#' Prepare HF water temperature for QC and assessment
#'
#' Applies the rule-based HF temperature QC steps: rebuilds each deployment's expected timestamp series at its mode sampling interval, linearly interpolates short gaps, and flags rate-of-change outliers with a z-score on sequential differences (stratified by winter vs. non-winter within each deployment).
#'
#' @param data data.frame of HF water temperature with at least \code{Sample_ID}, \code{HF_Location_ID}, \code{DateTime_Local}, and \code{Water_Measurement}. Columns that are constant within a Sample_ID (e.g. site attributes) are carried onto rebuilt timestamps.
#' @param max_gap_minutes Maximum time between the observations bracketing a gap for it to be interpolated. Default 30.
#' @param z_threshold Absolute z-score on sequential differences above which a record is flagged \code{is_outlier}. Default 9.5.
#' @param winter_months Months z-scored separately from the rest of the year. Default November-February.
#' @return data.table with the input columns plus \code{Mode_Time_Difference_Minutes}, \code{gap_minutes}, \code{QC_Measure_Val} (interpolated working value), \code{Interpolation_Flag}, \code{Water_Measurement_Diff}, \code{winter}, \code{z_score}, and \code{is_outlier}. \code{Water_Measurement} is left unchanged (NA on rebuilt timestamps).
#' @export prepHFTemp
prepHFTemp <- function(data, max_gap_minutes = 30, z_threshold = 9.5, winter_months = c(11, 12, 1, 2)) {
  if (!requireNamespace("zoo", quietly = TRUE)) stop("Package 'zoo' is required.")
  x <- data.table::copy(data.table::as.data.table(data))
  required <- c("Sample_ID", "HF_Location_ID", "DateTime_Local", "Water_Measurement")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  x <- x[!is.na(DateTime_Local)]
  data.table::setorder(x, Sample_ID, DateTime_Local)

  if (!"Mode_Time_Difference_Minutes" %in% names(x)) {
    x[, Mode_Time_Difference_Minutes := {
      d <- as.numeric(difftime(DateTime_Local, data.table::shift(DateTime_Local), units = "mins"))
      d <- d[is.finite(d) & d > 0]
      if (length(d)) as.numeric(names(which.max(table(d)))) else NA_real_
    }, by = Sample_ID]
  }

  message("Rebuilding expected timestamp series...")
  x <- x[, .hfFillTimeseries(.SD), by = Sample_ID, .SDcols = setdiff(names(x), "Sample_ID")]

  message("Interpolating gaps <= ", max_gap_minutes, " minutes...")
  x[, gap_minutes := .hfGapMinutes(DateTime_Local, Water_Measurement), by = Sample_ID]
  x[, QC_Measure_Val := Water_Measurement]
  x[, QC_Measure_Val := {
    can <- is.na(gap_minutes) | gap_minutes <= max_gap_minutes
    v <- Water_Measurement
    if (sum(!is.na(v)) >= 2) {
      filled <- zoo::na.approx(v, x = as.numeric(DateTime_Local), na.rm = FALSE)
      v[can] <- filled[can]
    }
    v
  }, by = Sample_ID]
  x[, Interpolation_Flag := data.table::fifelse(
    is.na(Water_Measurement) & !is.na(QC_Measure_Val), "Interpolated", NA_character_
  )]

  message("Flagging rate-of-change outliers (|z| > ", z_threshold, ")...")
  x[, Water_Measurement_Diff := QC_Measure_Val - data.table::shift(QC_Measure_Val), by = Sample_ID]
  x[, winter := as.integer(data.table::month(DateTime_Local) %in% winter_months)]
  x[, z_score := (Water_Measurement_Diff - mean(Water_Measurement_Diff, na.rm = TRUE)) /
    stats::sd(Water_Measurement_Diff, na.rm = TRUE), by = .(Sample_ID, winter)]
  x[, is_outlier := !is.na(z_score) & abs(z_score) > z_threshold]
  x[]
}

.hfFillTimeseries <- function(d) {
  d <- data.table::copy(d)
  interval <- stats::na.omit(unique(d$Mode_Time_Difference_Minutes))[1]
  if (is.na(interval) || !is.finite(interval) || interval <= 0) return(d)
  grid <- data.table::data.table(DateTime_Local = seq(
    min(d$DateTime_Local), max(d$DateTime_Local), by = paste(interval, "mins")
  ))
  # Keep off-grid observations (clock shifts, mixed intervals) alongside the expected grid.
  out <- merge(grid, d, by = "DateTime_Local", all = TRUE)
  constant <- names(d)[vapply(d, function(col) data.table::uniqueN(col, na.rm = TRUE) == 1L, logical(1))]
  for (col in setdiff(constant, c("DateTime_Local", "Water_Measurement"))) {
    data.table::set(out, j = col, value = rep(stats::na.omit(d[[col]])[1], nrow(out)))
  }
  out
}

# Minutes between the observations bracketing each run of missing values; NA for observed rows.
.hfGapMinutes <- function(datetime, value) {
  obs <- !is.na(value)
  block <- cumsum(obs)
  t_obs <- c(as.numeric(datetime)[obs], NA_real_)
  prev_t <- t_obs[pmax(block, 1L)]
  next_t <- t_obs[block + 1L]
  gap <- (next_t - prev_t) / 60
  gap[obs | block == 0L] <- NA_real_
  gap
}
