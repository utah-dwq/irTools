#' Flag potential buried-sensor days in HF water temperature for review
#'
#' Screens summer days for the buried-logger signature: a nearly flat daily trace while air temperature swings widely, sustained for several days, and flat compared with the same deployment's typical day. Days that pass every check for at least \code{min_run_days} consecutive days are written to a review queue. Nothing is rejected here; reviewers mark queued days \code{"Confirmed Buried"}, \code{"Confirmed Wet"}, or \code{"Unsure"}, and \code{applyHFTempQC()} removes Confirmed Buried days.
#'
#' A day is buried-like when all of these hold:
#' \itemize{
#'   \item daily water range below \code{max_range_c};
#'   \item daily air range at least \code{min_air_range_c} (a clear air swing to compare against);
#'   \item water range / air range below \code{max_range_ratio};
#'   \item water range below \code{max_rel_range} times the deployment's median daily range in the screened months.
#' }
#' The last check separates a buried episode from sites that are naturally damped all season (spring-fed reaches, dam tailwaters, snowmelt-dominated water). It also means a deployment that is buried for most of the season can be missed, because its own median range is flat; a nearby in-water reference is still the backstop for that case.
#'
#' \code{frac_flat} (share of consecutive readings that repeat exactly) and \code{longest_flat_h} are reported for review but are not part of the rule. Repeated values mostly reflect logger resolution: loggers and gages that report to 0.1 C repeat often in normal water.
#'
#' @param data HF water temperature with \code{Sample_ID}, \code{HF_Location_ID}, \code{DateTime_Local}, and the column named in \code{value_col}. Usually the output of \code{prepHFTemp()}.
#' @param air_temp Daily air temperature by site with \code{HF_Location_ID}, \code{Date} (or \code{date}), and \code{tmmn_mean}/\code{tmmx_mean} (or \code{tmmn_c}/\code{tmmx_c}) in degrees C. Same format as \code{flagDryHFTemp()}.
#' @param value_col Water temperature column. Default \code{"QC_Measure_Val"}.
#' @param months Months screened. Default May through September.
#' @param max_range_c Maximum daily water range (C) for a buried-like day. Default 1.
#' @param min_air_range_c Minimum daily air range (C) for a day to be screened. Default 10.
#' @param max_range_ratio Maximum water range / air range. Default 0.06.
#' @param max_rel_range Maximum water range relative to the deployment's median daily range. Default 0.33.
#' @param min_run_days Minimum consecutive buried-like days to queue for review. Default 3.
#' @param min_obs_per_day Minimum readings for a day to be screened. Default 12.
#' @param review_file Optional path for an .xlsx review file with sheets \code{review}, \code{runs}, and \code{settings}.
#' @return List with \code{daily} (every screened day), \code{runs} (one row per queued run), and \code{review} (queued days with empty \code{Review_Decision}, \code{Reviewer}, \code{Review_Comment}).
#' @export flagBuriedHFTemp
flagBuriedHFTemp <- function(data,
                             air_temp,
                             value_col = "QC_Measure_Val",
                             months = 5:9,
                             max_range_c = 1,
                             min_air_range_c = 10,
                             max_range_ratio = 0.06,
                             max_rel_range = 0.33,
                             min_run_days = 3,
                             min_obs_per_day = 12,
                             review_file = NULL) {
  hf <- data.table::as.data.table(data)
  missing_cols <- setdiff(c("Sample_ID", "HF_Location_ID", "DateTime_Local", value_col), names(hf))
  if (length(missing_cols)) stop("data is missing: ", paste(missing_cols, collapse = ", "))
  hf <- hf[!is.na(get(value_col)) & !is.na(DateTime_Local) &
    data.table::month(DateTime_Local) %in% months, .(
    Sample_ID, HF_Location_ID, DateTime_Local,
    Water_Measurement = get(value_col)
  )]
  if (!nrow(hf)) stop("No non-missing readings in months ", paste(months, collapse = ", "), ".")
  hf <- .hfJoinAirTemp(hf, air_temp)
  data.table::setorder(hf, Sample_ID, DateTime_Local)

  daily <- hf[, {
    v <- Water_Measurement
    d <- diff(v)
    step_min <- stats::median(diff(as.numeric(DateTime_Local)) / 60)
    same <- abs(d) < 0.001
    longest <- if (length(same) && any(same)) {
      r <- rle(same)
      max(r$lengths[r$values]) * step_min / 60
    } else {
      0
    }
    .(
      n_rec = .N,
      water_temp_mean = mean(v),
      range_d = max(v) - min(v),
      sd_d = if (.N > 1) stats::sd(v) else NA_real_,
      frac_flat = if (length(d)) mean(same) else NA_real_,
      longest_flat_h = longest,
      air_temp_min = data.table::first(tmmn_mean),
      air_temp_max = data.table::first(tmmx_mean)
    )
  }, by = .(Sample_ID, HF_Location_ID, Date)]

  daily[, air_temp_range := air_temp_max - air_temp_min]
  daily[, range_ratio := data.table::fifelse(air_temp_range > 0, range_d / air_temp_range, NA_real_)]
  daily[, screened := n_rec >= min_obs_per_day & is.finite(air_temp_range)]
  daily[, deployment_median_range := stats::median(range_d[screened]), by = Sample_ID]
  daily[, rel_range := data.table::fifelse(
    deployment_median_range > 0, range_d / deployment_median_range, NA_real_
  )]
  daily[, buried_like := screened &
    range_d < max_range_c &
    air_temp_range >= min_air_range_c &
    !is.na(range_ratio) & range_ratio < max_range_ratio &
    !is.na(rel_range) & rel_range < max_rel_range]

  data.table::setorder(daily, Sample_ID, Date)
  daily[, run_id := cumsum(
    buried_like != data.table::shift(buried_like, fill = !buried_like[1]) |
      as.integer(Date - data.table::shift(Date, fill = Date[1] - 1L)) != 1L
  ), by = Sample_ID]
  daily[, run_days := data.table::fifelse(buried_like, .N, 0L), by = .(Sample_ID, run_id)]
  daily[, review_disposition := data.table::fcase(
    !screened, "Not screened - coverage or air",
    buried_like & run_days >= min_run_days, "Buried review",
    buried_like, "Flat - short run",
    default = "Retain"
  )]

  runs <- daily[review_disposition == "Buried review", .(
    start_date = min(Date),
    end_date = max(Date),
    n_days = .N,
    median_range_d = stats::median(range_d),
    median_air_range = stats::median(air_temp_range),
    median_range_ratio = stats::median(range_ratio),
    median_rel_range = stats::median(rel_range),
    deployment_median_range = data.table::first(deployment_median_range),
    median_frac_flat = stats::median(frac_flat)
  ), by = .(Sample_ID, HF_Location_ID, run_id)]
  runs[, run_id := NULL]

  review <- daily[review_disposition == "Buried review"]
  review[, `:=`(
    Check_Version = "buried_screen_v1",
    Review_Decision = NA_character_,
    Reviewer = NA_character_,
    Review_Comment = NA_character_
  )]

  message(
    "Buried review queue: ", nrow(review), " days in ", nrow(runs), " run(s) across ",
    data.table::uniqueN(review$Sample_ID), " Sample_IDs."
  )
  print(table(daily$review_disposition))

  if (!is.null(review_file)) {
    settings <- data.frame(
      field = c(
        "check_version", "months", "max_range_c", "min_air_range_c", "max_range_ratio",
        "max_rel_range", "min_run_days", "min_obs_per_day", "value_col", "screened_at"
      ),
      value = c(
        "buried_screen_v1", paste(months, collapse = ","), max_range_c, min_air_range_c,
        max_range_ratio, max_rel_range, min_run_days, min_obs_per_day, value_col,
        as.character(Sys.time())
      )
    )
    writexl::write_xlsx(
      list(review = as.data.frame(review), runs = as.data.frame(runs), settings = settings),
      path = review_file
    )
    message("Wrote review file: ", review_file)
  }

  list(daily = daily, runs = runs, review = review)
}
