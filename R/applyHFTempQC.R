#' Apply reviewed dry- and buried-sensor decisions to HF water temperature
#'
#' Removes the deployment-days a reviewer marked \code{"Confirmed Dry"} (review file from \code{flagDryHFTemp()}) or \code{"Confirmed Buried"} (review file from \code{flagBuriedHFTemp()}). \code{"Confirmed Wet"} (sensor was in the stream and the data are representative) and \code{"Unsure"} days are kept, as are days that were never queued. Model scores, the physics air-like flag, and the buried screen never remove data on their own; only a reviewer's decision does. Run after review and before \code{assessHFTemp()}.
#'
#' If \code{data} already has a \code{reject_reason} column (for example the \code{data} element returned by highfreqQC's \code{applyHFTempRejectRules()}), existing reasons are kept and only unrejected rows can pick up a reviewed reason.
#'
#' @param data HF water temperature with \code{Sample_ID} and \code{DateTime_Local} (or \code{Date}).
#' @param review Completed review table(s): the \code{review} element of \code{flagDryHFTemp()} or \code{flagBuriedHFTemp()} after reviewers fill \code{Review_Decision}, a path to an .xlsx review file (sheet \code{review}), or a list / character vector of these to apply several review files at once. Needs \code{Sample_ID}, \code{Date}, and \code{Review_Decision}.
#' @return List with \code{accepted} (rows to assess), \code{rejected} (rows removed, with \code{reject_reason}), and \code{data} (all rows with \code{reject_reason} and \code{reject_flag}).
#' @export applyHFTempQC
applyHFTempQC <- function(data, review) {
  dt <- data.table::copy(data.table::as.data.table(data))
  if (!"Sample_ID" %in% names(dt)) stop("data is missing Sample_ID.")
  dt_col <- if ("DateTime_Local" %in% names(dt)) {
    "DateTime_Local"
  } else if ("Date" %in% names(dt)) {
    "Date"
  } else {
    stop("data needs DateTime_Local or Date.")
  }

  rv <- .hfReadReviews(review)
  missing_cols <- setdiff(c("Sample_ID", "Date", "Review_Decision"), names(rv))
  if (length(missing_cols)) stop("review is missing: ", paste(missing_cols, collapse = ", "))

  rv[, Review_Decision := trimws(as.character(Review_Decision))]
  rv[Review_Decision == "", Review_Decision := NA_character_]
  allowed <- c("Confirmed Dry", "Confirmed Buried", "Confirmed Wet", "Unsure")
  bad <- setdiff(stats::na.omit(unique(rv$Review_Decision)), allowed)
  if (length(bad)) {
    stop(
      "Unrecognized Review_Decision value(s): ", paste(bad, collapse = ", "),
      ". Use one of: ", paste(allowed, collapse = ", "), "."
    )
  }
  n_open <- sum(is.na(rv$Review_Decision))
  if (n_open) warning(n_open, " queued day(s) have no Review_Decision and are kept.")

  dry_reason <- "Reviewed dry sensor"
  if ("ML_Model_Version" %in% names(rv)) {
    versions <- stats::na.omit(unique(as.character(rv[Review_Decision == "Confirmed Dry", ML_Model_Version])))
    if (length(versions)) dry_reason <- paste0(dry_reason, " (", paste(versions, collapse = ", "), ")")
  }
  rv[, .reason := data.table::fcase(
    Review_Decision == "Confirmed Dry", dry_reason,
    Review_Decision == "Confirmed Buried", "Reviewed buried sensor",
    default = NA_character_
  )]
  hits <- unique(rv[!is.na(.reason), .(
    .key_id = as.character(Sample_ID),
    .key_date = data.table::as.IDate(as.Date(Date)),
    hit_reason = .reason
  )], by = c(".key_id", ".key_date"))

  dt[, .key_id := as.character(Sample_ID)]
  dt[, .key_date := data.table::as.IDate(get(dt_col))]
  dt[, .hit_reason := NA_character_]
  dt[hits, on = .(.key_id, .key_date), .hit_reason := i.hit_reason]

  if (!"reject_reason" %in% names(dt)) dt[, reject_reason := NA_character_]
  n_new <- sum(!is.na(dt$.hit_reason) & is.na(dt$reject_reason))
  dt[!is.na(.hit_reason) & is.na(reject_reason), reject_reason := .hit_reason]
  dt[, reject_flag := data.table::fifelse(!is.na(reject_reason), "REJECT", NA_character_)]

  n_matched <- data.table::uniqueN(dt[!is.na(.hit_reason), .(.key_id, .key_date)])
  if (n_matched < nrow(hits)) {
    warning(nrow(hits) - n_matched, " confirmed day(s) did not match any row in data.")
  }
  message(
    "Confirmed Dry: ", rv[Review_Decision == "Confirmed Dry", data.table::uniqueN(paste(Sample_ID, Date))],
    " day(s); Confirmed Buried: ", rv[Review_Decision == "Confirmed Buried", data.table::uniqueN(paste(Sample_ID, Date))],
    " day(s); ", n_new, " row(s) newly rejected."
  )

  dt[, c(".key_id", ".key_date", ".hit_reason") := NULL]
  list(
    accepted = dt[is.na(reject_flag)],
    rejected = dt[!is.na(reject_flag)],
    data = dt
  )
}

.hfReadReviews <- function(review) {
  items <- if (is.data.frame(review)) list(review) else as.list(review)
  tabs <- lapply(items, function(x) {
    if (is.character(x)) {
      if (!file.exists(x)) stop("Review file not found: ", x)
      x <- readxl::read_excel(x, sheet = "review")
    }
    x <- data.table::as.data.table(x)
    if ("Date" %in% names(x)) x[, Date := data.table::as.IDate(as.Date(Date))]
    if ("Sample_ID" %in% names(x)) x[, Sample_ID := as.character(Sample_ID)]
    if ("Review_Decision" %in% names(x)) x[, Review_Decision := as.character(Review_Decision)]
    x
  })
  data.table::rbindlist(tabs, use.names = TRUE, fill = TRUE)
}
