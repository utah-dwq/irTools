#' Run high-frequency temperature assessments
#'
#' 10% excursion assessments for continuous water temperature, matching the
#' IR-2026 production method: individual critical-season readings versus the
#' numeric max-temperature criterion (typically 20 C for 3A, 27 C for 3B/3C).
#' An assessment unit is NS if any site exceeds `exceed_frac`.
#'
#' This function assesses **already accepted** data. Interpolate, Dry-sensor
#' scoring, human review, and REJECT rules live in the `highfreqQC` repo
#' (`prepHFTemp()`, `flagDryHFTemp()`, `applyHFTempRejectRules()`).
#'
#' @param data Accepted HF temperature records with a measurement column,
#'   a numeric `Criteria` column, timestamps, and site/AU identifiers.
#' @param value_col Measurement column. Default `Water_Measurement`.
#' @param criteria_col Numeric criterion column. Default `Criteria`.
#' @param exceed_frac Exceedance fraction for NS. Default 0.10.
#' @param start_month,start_day,end_month,end_day Critical season (default
#'   May 15–September 30).
#' @return List with `site_assessments` and `au_assessments`.
#' @export assessHFTemp
assessHFTemp <- function(data,
                         value_col = "Water_Measurement",
                         criteria_col = "Criteria",
                         exceed_frac = 0.10,
                         start_month = 5,
                         start_day = 15,
                         end_month = 9,
                         end_day = 30) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  if (!value_col %in% names(data)) {
    stop("assessHFTemp() missing value column: ", value_col)
  }
  if (!criteria_col %in% names(data)) {
    stop(
      "assessHFTemp() missing criteria column: ", criteria_col,
      ". Assign numeric criteria before assessing."
    )
  }

  dt_col <- if ("DateTime_Local" %in% names(data)) {
    "DateTime_Local"
  } else if ("Date" %in% names(data)) {
    "Date"
  } else {
    stop("Need DateTime_Local or Date.")
  }

  x <- data
  x$.assess_date <- as.Date(x[[dt_col]])
  md <- 100 * as.integer(format(x$.assess_date, "%m")) +
    as.integer(format(x$.assess_date, "%d"))
  start <- 100 * as.integer(start_month) + as.integer(start_day)
  end <- 100 * as.integer(end_month) + as.integer(end_day)
  x <- x[!is.na(x[[value_col]]) & md >= start & md <= end, , drop = FALSE]

  group_cols <- intersect(
    c(
      "IR_MLNAME", "IR_MLID", "HF_Location_ID", "Sample_ID",
      "IR_Long", "IR_Lat", "ASSESS_ID", "AU_NAME",
      "BeneficialUse", "BEN_CLASS", criteria_col
    ),
    names(x)
  )
  if (length(group_cols) == 0) {
    stop("No grouping columns found for site-level assessment.")
  }

  val <- x[[value_col]]
  crit <- x[[criteria_col]]
  x$.exceed <- val > crit

  site <- dplyr::summarise(
    dplyr::group_by_at(x, group_cols),
    SampleCount = dplyr::n(),
    DayCount = length(unique(.assess_date)),
    ExcCount = sum(.exceed, na.rm = TRUE),
    Percent_Exceed = round(sum(.exceed, na.rm = TRUE) / dplyr::n(), 2),
    min_month = min(as.integer(format(.assess_date, "%m")), na.rm = TRUE),
    max_month = max(as.integer(format(.assess_date, "%m")), na.rm = TRUE),
    .groups = "drop"
  )
  site$MLID_Cat <- ifelse(site$Percent_Exceed > exceed_frac, "NS", "FS")

  au_cols <- intersect(c("ASSESS_ID", "AU_NAME", "BeneficialUse"), names(site))
  if (length(au_cols) == 0) {
    au <- data.frame(
      AU_Cat = ifelse(any(site$MLID_Cat == "NS"), "NS", "FS"),
      stringsAsFactors = FALSE
    )
  } else {
    au <- dplyr::summarise(
      dplyr::group_by_at(site, au_cols),
      AU_Cat = ifelse(any(MLID_Cat == "NS"), "NS", "FS"),
      .groups = "drop"
    )
    site <- dplyr::left_join(site, au, by = au_cols)
  }

  list(
    site_assessments = as.data.frame(site),
    au_assessments = as.data.frame(au)
  )
}