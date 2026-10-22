#' Load the dry-sensor random forest
#'
#' Loads the dry-sensor random forest saved by the highfreqQC training workflow. The file must contain \code{rf_model}, one trained \code{randomForest}. A random forest is already an ensemble of trees, so review scores come from that single forest. Files that hold a \code{model_list} of several forests are retired artifacts and are refused.
#'
#' If the file also contains \code{model_manifest}, its feature version and predictors must match \code{hfTempFeatureVersion()} and \code{hfTempPredictorCols()}, so training and scoring cannot silently drift apart. Without a manifest, the model version is taken from the file name and review thresholds use the defaults in \code{flagDryHFTemp()}.
#'
#' @param path Path to the model \code{.RData} file.
#' @return List with \code{model} (one \code{randomForest}) and \code{manifest}.
#' @export loadDryModel
loadDryModel <- function(path) {
  if (!file.exists(path)) stop("Model file not found: ", path)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)

  if (!exists("rf_model", envir = env) && exists("model_list", envir = env)) {
    stop(
      "Model file ", path, " is the retired multi-forest list. ",
      "Retrain in highfreqQC so scoring uses one random forest (rf_model)."
    )
  }
  if (!exists("rf_model", envir = env)) stop("No 'rf_model' object in ", path)
  model <- get("rf_model", envir = env)
  if (is.list(model) && !inherits(model, "randomForest")) {
    stop("'rf_model' in ", path, " is a list of forests. Scoring expects one randomForest.")
  }
  if (!inherits(model, "randomForest")) stop("'rf_model' in ", path, " is not a randomForest.")

  manifest <- if (exists("model_manifest", envir = env)) get("model_manifest", envir = env) else NULL
  if (is.null(manifest)) {
    manifest <- list(
      model_version = tools::file_path_sans_ext(basename(path)),
      feature_version = NA_character_
    )
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
  list(model = model, manifest = manifest)
}

#' Score daily dry-sensor probabilities with one random forest
#'
#' @param features Daily feature table from \code{buildHFTempFeatures()}.
#' @param model One trained \code{randomForest}, e.g. \code{loadDryModel(path)$model}.
#' @param threshold Probability at or above which a day is predicted Dry. Default 0.90.
#' @return data.table with \code{Sample_ID}, \code{Date}, \code{avg_prob} (the forest's Dry probability), \code{predictedState}, and \code{probValues}. Days missing any predictor are dropped.
#' @export predictDryHFTemp
predictDryHFTemp <- function(features, model, threshold = 0.90) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package 'randomForest' is required to score dry-sensor models.")
  }
  if (missing(model) || is.null(model)) stop("`model` must be one trained randomForest.")
  if (is.list(model) && !inherits(model, "randomForest")) {
    stop("predictDryHFTemp() takes one random forest, not a list of forests.")
  }
  if (!inherits(model, "randomForest")) stop("`model` must be a randomForest object.")

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

  probs <- stats::predict(model, test_data_full, type = "prob")[, "Dry"]
  data.table::data.table(
    Sample_ID = test_data_full$Sample_ID,
    Date = test_data_full$Date,
    avg_prob = probs,
    predictedState = ifelse(probs >= threshold, "Dry", "Wet"),
    probValues = sprintf("%.2f", probs)
  )
}
