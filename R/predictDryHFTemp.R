#' Load a versioned dry-sensor model
#'
#' Loads a dry-sensor Random Forest ensemble saved by the highfreqQC training workflow (\code{models/<model_version>/dry_model.RData}). The file must contain \code{model_list}; a \code{model_manifest} list records the feature version, predictors, and review thresholds the model was trained with. Models trained on a different feature version than \code{hfTempFeatureVersion()} are refused so training and scoring cannot silently drift apart.
#'
#' @param path Path to the model \code{.RData} file.
#' @param require_manifest Logical. If TRUE (default), stop when the file has no \code{model_manifest}. Set FALSE only to score with a legacy unversioned model.
#' @return List with \code{model_list} and \code{manifest}.
#' @export loadDryModel
loadDryModel <- function(path, require_manifest = TRUE) {
  if (!file.exists(path)) stop("Model file not found: ", path)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists("model_list", envir = env)) stop("No 'model_list' object in ", path)

  manifest <- if (exists("model_manifest", envir = env)) get("model_manifest", envir = env) else NULL
  if (is.null(manifest)) {
    if (require_manifest) {
      stop("No 'model_manifest' in ", path, ". Register the model in highfreqQC/models or set require_manifest = FALSE.")
    }
    warning("Scoring with an unversioned model; review thresholds fall back to defaults.")
    manifest <- list(model_version = basename(path), feature_version = NA_character_)
  } else {
    if (!identical(manifest$feature_version, hfTempFeatureVersion())) {
      stop(
        "Model ", manifest$model_version, " was trained on features '", manifest$feature_version,
        "' but irTools builds '", hfTempFeatureVersion(), "'. Retrain or install the matching irTools version."
      )
    }
    if (!identical(sort(manifest$predictor_cols), sort(hfTempPredictorCols()))) {
      stop("Model ", manifest$model_version, " predictors do not match hfTempPredictorCols().")
    }
  }
  list(model_list = get("model_list", envir = env), manifest = manifest)
}

#' Score daily dry-sensor probabilities with a Random Forest ensemble
#'
#' @param features Daily feature table from \code{buildHFTempFeatures()}.
#' @param model_list List of trained \code{randomForest} models.
#' @param threshold Probability at or above which a day is predicted Dry. Default 0.90.
#' @param decision Ensemble rule: \code{"avg"} (mean probability, default), \code{"majority"}, or \code{"any"}.
#' @return data.table with Sample_ID, Date, per-model probabilities, \code{avg_prob}, and \code{predictedState}. Days missing any predictor are dropped.
#' @export predictDryHFTemp
predictDryHFTemp <- function(features, model_list, threshold = 0.90, decision = c("avg", "majority", "any")) {
  decision <- match.arg(decision)
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required to score dry-sensor models.")
  }
  if (length(model_list) < 1) stop("`model_list` must be a non-empty list of randomForest models.")

  features <- data.table::as.data.table(features)
  predictor_cols <- hfTempPredictorCols()
  missing_cols <- setdiff(predictor_cols, names(features))
  if (length(missing_cols) > 0) {
    stop("Missing predictor columns: ", paste(missing_cols, collapse = ", "))
  }

  test_data_full <- features[
    stats::complete.cases(features[, ..predictor_cols]) &
      is.finite(mod_zscore_7d) &
      is.finite(mod_zscore_global)
  ]
  if (nrow(test_data_full) == 0) {
    warning("No complete cases available for prediction.")
    return(data.table::data.table())
  }

  n_models <- length(model_list)
  prob_cols <- paste0("M", seq_len(n_models), "_prob")
  flag_cols <- paste0("M", seq_len(n_models), "_flag")

  out <- data.table::data.table(Sample_ID = test_data_full$Sample_ID, Date = test_data_full$Date)
  for (i in seq_len(n_models)) {
    probs <- stats::predict(model_list[[i]], test_data_full, type = "prob")[, "Dry"]
    data.table::set(out, j = prob_cols[i], value = probs)
    data.table::set(out, j = flag_cols[i], value = probs >= threshold)
  }

  out[, avg_prob := rowMeans(.SD), .SDcols = prob_cols]
  out[, dryCounts := rowSums(.SD), .SDcols = flag_cols]
  out[, predictedState_majority := ifelse(dryCounts >= 2, "Dry", "Wet")]
  out[, predictedState_any := ifelse(dryCounts > 0, "Dry", "Wet")]
  out[, predictedState_avg := ifelse(avg_prob >= threshold, "Dry", "Wet")]
  out[, predictedState := switch(decision,
    avg = predictedState_avg,
    majority = predictedState_majority,
    any = predictedState_any
  )]
  out[, probValues := apply(.SD, 1, function(x) paste(round(x, 2), collapse = ", ")), .SDcols = prob_cols]
  out
}
