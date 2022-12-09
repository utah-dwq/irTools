#' Prep WQP Data
#'
#' This function reads dataframes downloaded from EPA's WQP, removes duplicates, and converts result values and detection limit values with non-numeric characters to NA (with the exception of those with <|>|,).
#' Converts all non-result value columns in nr, activity, detquantlim, and sites objects to character, including dates.
#' Replaces hard coding in bookdown and readWQPFiles function.
#'
#' @param irdata A list of WQP dataframes containing sites, narrowresult, activity, and detquantlim objects.
#' @param char_check Logical. If TRUE, the function produces a .csv showing all the special characters changed or converted to NA.
#' @return A list consisting of nr, activity, detquantlim, sites, filtered sites, and merged_results (nr combined with activity) dataframes with special characters converted to NA for the ResultMeasureValue column. Saves an rdata file named "irdata_prepped.Rdata" containing the six data WQP objects.

#' @export
prepWQPData <- function(irdata, char_check=TRUE){
  
  # Get unique records - WQP had some duplication issues in 04/2022
  print("Reading in dataframes, removing duplicate values, converting all columns to character...")
  
  nr = unique(irdata$nr)
  nr[] = lapply(nr,as.character)
  activity = unique(irdata$activity)
  activity[] = lapply(activity,as.character)
  detquantlim = unique(irdata$detquantlim)
  detquantlim[] = lapply(detquantlim,as.character)
  sites = unique(irdata$sites)
  sites[] = lapply(sites,as.character)
  
  print("In narrowresult and detquantlim, converting result values to numeric and character strings to NA...")
  # Create original raw value column 
  nr$ResultMeasureValue_raw = nr$ResultMeasureValue
  # chars = unique(nr[,"ResultMeasureValue"][which(is.na(suppressWarnings(as.numeric(nr[,"ResultMeasureValue"]))))])
  # Remove commas, inequality signs from result value column.
  nr$ResultMeasureValue = gsub(",|<|>","",nr$ResultMeasureValue)
  # chars1 = unique(nr[,"ResultMeasureValue"][which(is.na(suppressWarnings(as.numeric(nr[,"ResultMeasureValue"]))))])
  # Convert result values to numeric - results in NAs for values containing characters.
  nr$ResultMeasureValue = suppressWarnings(wqTools::facToNum(nr$ResultMeasureValue))
  
  # Create original raw value column 
  detquantlim$DetectionQuantitationLimitMeasure.MeasureValue_raw = detquantlim$DetectionQuantitationLimitMeasure.MeasureValue
  # Remove commas, inequality signs from result value column.
  detquantlim$DetectionQuantitationLimitMeasure.MeasureValue = gsub(",|<|>","",detquantlim$DetectionQuantitationLimitMeasure.MeasureValue)
  # Convert result values to numeric - results in NAs for values containing characters.
  detquantlim$DetectionQuantitationLimitMeasure.MeasureValue = suppressWarnings(wqTools::facToNum(detquantlim$DetectionQuantitationLimitMeasure.MeasureValue))
  
  print("Filtering to sites with data, converting latitude and longitude measures to numeric, composing a merged results object, saving irdata object to working directory.")
  # filter to sites with data
  sites_filtered = subset(sites, sites$MonitoringLocationIdentifier%in%unique(nr$MonitoringLocationIdentifier))
  sites_filtered$LatitudeMeasure = suppressWarnings(wqTools::facToNum(sites_filtered$LatitudeMeasure))
  sites_filtered$LongitudeMeasure = suppressWarnings(wqTools::facToNum(sites_filtered$LongitudeMeasure))
  
  merged_results = merge(nr, activity, all.x = TRUE)
  
  ### Checks - if true, produces a csv showing which unique values were converted to NA
  if(char_check==TRUE){
    nr_check = subset(nr, !is.na(nr$ResultMeasureValue_raw))
    nr_check = unique(nr_check[,c("ResultIdentifier","ResultMeasureValue_raw","ResultMeasureValue","CharacteristicName")])
    nr_check = subset(nr_check, is.na(nr_check$ResultMeasureValue)|grepl(",|<|>", nr_check$ResultMeasureValue_raw))
    nr_check$ResultMeasureValue[is.na(nr_check$ResultMeasureValue)]="NA"
    write.csv(nr_check, "special_char_check.csv", row.names = FALSE)
    print("Special character check completed. 'special_char_check.csv' saved to working directory.")
  }
  # built irdata list of dataframes from WQP
  irdata = list(nr=nr,activity=activity,detquantlim=detquantlim,sites=sites,sites_filtered=sites_filtered, merged_results=merged_results)
  save(irdata, file = "irdata_prepped.Rdata")
  
  return(irdata)
}
