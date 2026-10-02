#' Flag potential dry-sensor days in HF water temperature for review
#'
#' Builds daily features (\code{buildHFTempFeatures}), scores them with a versioned dry-sensor model, adds harmonic physics evidence, and assigns a review disposition to every day. Neither the model nor physics rejects data; days needing review are written to a review file (analogous to \code{validateSites()} output for \code{siteValApp()}). A random sample of low-score days is added as an audit so missed dry days can be measured.
#'
#' Reviewers fill \code{Review_Decision} with one of \code{"Confirmed Dry"}, \code{"Confirmed Wet"}, or \code{"Unsure"}. Confirmed Dry days are rejected by \code{applyHFTempQC()}, and confirmed decisions become candidate training labels for the next model retrain in highfreqQC.
#'
#' @param data Output of \code{prepHFTemp()}.
#' @param air_temp Daily air temperature by site with \code{HF_Location_ID}, \code{Date} (or \code{date}), and \code{tmmn_mean}/\code{tmmx_mean} (or \code{tmmn_c}/\code{tmmx_c}) in degrees C.
#' @param model Output of \code{loadDryModel()}, or a path to a model file.
#' @param value_col Water temperature column used to build features. Default \code{"QC_Measure_Val"} (interpolated working value).
#' @param review_file Optional path for an .xlsx review file with sheets \code{review} and \code{model}.
#' @param audit_n Number of randomly selected \code{"Retain"} days added to the review as \code{"Audit - low score"}. Default 25.
#' @param seed Random seed for the audit sample.
#' @return List with \code{daily} (every scored day), \code{review} (days needing review), and \code{manifest} (model metadata).
#' @export flagDryHFTemp
flagDryHFTemp <- function(data, air_temp, model, value_col = "QC_Measure_Val", review_file = NULL, audit_n = 25, seed = 2028) {
  if (is.character(model)) model <- loadDryModel(model)
  manifest <- model$manifest
  policy <- .hfDryPolicy(manifest)

  hf <- data.table::as.data.table(data)
  if (!value_col %in% names(hf)) stop("Column '", value_col, "' not found in data.")
  hf <- hf[, .(
    Sample_ID, HF_Location_ID, DateTime_Local,
    Water_Measurement = get(value_col)
  )]
  hf[, Date := data.table::as.IDate(DateTime_Local)]

  air <- data.table::copy(data.table::as.data.table(air_temp))
  if (!"Date" %in% names(air) && "date" %in% names(air)) data.table::setnames(air, "date", "Date")
  if (!"tmmn_mean" %in% names(air) && "tmmn_c" %in% names(air)) data.table::setnames(air, "tmmn_c", "tmmn_mean")
  if (!"tmmx_mean" %in% names(air) && "tmmx_c" %in% names(air)) data.table::setnames(air, "tmmx_c", "tmmx_mean")
  missing_air <- setdiff(c("HF_Location_ID", "Date", "tmmn_mean", "tmmx_mean"), names(air))
  if (length(missing_air)) stop("air_temp is missing: ", paste(missing_air, collapse = ", "))
  air <- unique(air[, .(
    HF_Location_ID = as.character(HF_Location_ID),
    Date = data.table::as.IDate(Date),
    tmmn_mean, tmmx_mean
  )], by = c("HF_Location_ID", "Date"))
  hf[, HF_Location_ID := as.character(HF_Location_ID)]
  hf <- merge(hf, air, by = c("HF_Location_ID", "Date"), all.x = TRUE)
  hf[, Date := NULL]

  features <- buildHFTempFeatures(hf, air_max_hour = policy$air_max_hour_local)
  scores <- predictDryHFTemp(features, model$model_list, threshold = policy$rf_review_threshold, decision = "avg")

  site_ids <- unique(hf[, .(Sample_ID, HF_Location_ID)], by = "Sample_ID")
  daily <- features[, .(Sample_ID, Date, water_temp_max, range_d, air_temp_min, air_temp_max, amplitude_ratio, phase_lag_hours)]
  if (nrow(scores)) {
    daily <- merge(daily, scores[, .(Sample_ID, Date, avg_prob, probValues)], by = c("Sample_ID", "Date"), all.x = TRUE)
  } else {
    daily[, `:=`(avg_prob = NA_real_, probValues = NA_character_)]
  }
  daily <- merge(site_ids, daily, by = "Sample_ID", all.y = TRUE)

  physics <- is.finite(daily$amplitude_ratio) &
    daily$amplitude_ratio >= policy$amplitude_ratio_min &
    daily$amplitude_ratio <= policy$amplitude_ratio_max &
    is.finite(daily$phase_lag_hours) &
    abs(daily$phase_lag_hours) <= policy$abs_phase_lag_max &
    is.finite(daily$range_d) &
    daily$range_d >= policy$min_temp_range_c
  p <- data.table::fifelse(is.na(daily$avg_prob), 0, daily$avg_prob)
  daily[, physics_air_like := physics]
  daily[, review_disposition := data.table::fcase(
    physics & p >= policy$rf_review_threshold, "High-confidence Dry review",
    physics, "Physics Dry review",
    p >= policy$rf_review_threshold, "RF-only review",
    p >= policy$rf_secondary_threshold, "Borderline - retain",
    default = "Retain"
  )]
  daily[is.na(avg_prob) & review_disposition == "Retain", review_disposition := "Not scored - missing features"]

  queue_states <- c("High-confidence Dry review", "Physics Dry review", "RF-only review")
  retain_idx <- which(daily$review_disposition == "Retain")
  if (audit_n > 0 && length(retain_idx)) {
    set.seed(seed)
    audit_idx <- retain_idx[sample.int(length(retain_idx), min(audit_n, length(retain_idx)))]
    daily[audit_idx, review_disposition := "Audit - low score"]
    queue_states <- c(queue_states, "Audit - low score")
  }
  daily[, ML_Model_Version := manifest$model_version]
  data.table::setorder(daily, Sample_ID, Date)

  review <- daily[review_disposition %in% queue_states]
  review[, `:=`(Review_Decision = NA_character_, Reviewer = NA_character_, Review_Comment = NA_character_)]

  message(
    "Dry review queue: ", nrow(review), " days across ", data.table::uniqueN(review$Sample_ID),
    " Sample_IDs (model ", manifest$model_version, ")."
  )
  print(table(daily$review_disposition))

  if (!is.null(review_file)) {
    model_sheet <- data.frame(
      field = c("model_version", "feature_version", "trained_at", "rf_review_threshold", "rf_secondary_threshold", "scored_at"),
      value = c(
        as.character(manifest$model_version), as.character(manifest$feature_version),
        as.character(manifest$trained_at), policy$rf_review_threshold, policy$rf_secondary_threshold,
        as.character(Sys.time())
      )
    )
    writexl::write_xlsx(list(review = as.data.frame(review), model = model_sheet), path = review_file)
    message("Wrote review file: ", review_file)
  }

  list(daily = daily, review = review, manifest = manifest)
}

.hfDryPolicy <- function(manifest) {
  defaults <- list(
    air_max_hour_local = 15,
    amplitude_ratio_min = 0.65,
    amplitude_ratio_max = 1.45,
    abs_phase_lag_max = 2,
    min_temp_range_c = 8,
    rf_review_threshold = 0.90,
    rf_secondary_threshold = 0.80
  )
  saved <- manifest$policy
  if (is.null(saved)) saved <- list()
  if (!is.null(manifest$review_threshold)) saved$rf_review_threshold <- manifest$review_threshold
  utils::modifyList(defaults, saved[intersect(names(saved), names(defaults))])
}
