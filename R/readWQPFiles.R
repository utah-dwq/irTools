#' Read WQP Files
#' 
#' Creates list object containing data frames needed to run irTools. Removes duplicates and creates objects containing orphan records. Identifies non-numeric values in numeric columns and allows user to edit values and save to new object.
#' @param file_select Logical. If TRUE, navigation window will pop up for manual selection of files. If FALSE, function requires full path names to files.
#' @param sites_file Full path and filename of sites file queried from WQP to be reviewed (.csv).
#' @param activity_file Full path and filename of activity file queried from WQP to be reviewed (.csv).
#' @param narrowresult_file Full path and filename of narrowresult file queried from WQP to be reviewed (.csv).
#' @param detquantlim_file Full path and filename of detquantlim file queried from WQP to be reviewed (.csv).
#' @param orph_check Logical. Specifies whether function should perform orphan check between narrowresult and sites, activity, and detquantlim. If TRUE, produces list object containing orphan rows.
#' @importFrom wqTools facToNum
#' @return List containing dataframes needed to run irTools package, as well as orphan records and edited non-numeric data tables.

# num_check Logical. Specifies whether function should perform checks for non-numeric values in numeric columns (ResultMeasureValue and DetectionQuantitationLimitMeasure.MeasureValue). Opens edit window to make changes to non-numeric values in the data.


## Testing Function ##

# wqpdat <- readWQPFiles("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\narrowresult061001-080930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\sites061001-080930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\activity061001-080930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\detquantlim061001-080930.csv",
#             orph_check = TRUE)

#' @export
readWQPFiles <- function(file_select,narrowresult_file, sites_file, activity_file, detquantlim_file, orph_check=TRUE){

## Testing setup ##
# file_select = FALSE
# orph_check = FALSE
# sites_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\sites061001-080930.csv"
# narrowresult_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\narrowresult061001-080930.csv"
# activity_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\activity061001-080930.csv"
# detquantlim_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\detquantlim061001-080930.csv"

wqpdat <- list()

print("------------READING IN FILES--------------")
if(file_select){
narrowresult=read.csv(choose.files(getwd(), multi=F, caption="Select narrow result file..."),colClasses=c("ResultMeasureValue"="factor"), na.strings=c(""," ","NA"))
activity=read.csv(choose.files(getwd(), multi=F, caption="Select activity file..."), na.strings=c(""," ","NA"))
wqpdat$sites <- read.csv(choose.files(getwd(), multi=F, caption="Select sites file..."), na.strings=c(""," ","NA"))#, strip.white = TRUE)
wqpdat$detquantlim <- read.csv(choose.files(getwd(), multi=F, caption="Select detection/quantitation limit file..."), colClasses=c("DetectionQuantitationLimitMeasure.MeasureValue"="factor"),na.strings=c(""," ","NA"))#, strip.white = TRUE)
}else{
narrowresult <- read.csv(narrowresult_file, colClasses=c("ResultMeasureValue"="factor"),na.strings=c(""," ","NA"))#, strip.white = TRUE)
activity <- read.csv(activity_file, na.strings=c(""," ","NA"))#, strip.white = TRUE)
wqpdat$sites <- read.csv(sites_file, na.strings=c(""," ","NA"))#, strip.white = TRUE)
wqpdat$detquantlim <- read.csv(detquantlim_file, colClasses=c("DetectionQuantitationLimitMeasure.MeasureValue"="factor"), na.strings=c(""," ","NA"))#, strip.white = TRUE)
}
  
### Check for duplicates ###
print("----REMOVING EXACT DUPLICATES-----")
narrowresult <- unique(narrowresult)
wqpdat$sites <- unique(wqpdat$sites)
activity <- unique(activity)
wqpdat$detquantlim <- unique(wqpdat$detquantlim)

### Create merged_results object ###
wqpdat$merged_results=merge(narrowresult,activity,all.x=T)

### Check for orphans ###
if(orph_check){
  print("-----PERFORMING ORPHAN RECORD CHECKS------")
  orph_check <- function(data1,data2){
    # data1 = narrowresult
    # data2 = detquantlim
    dat1name <- deparse(substitute(data1))
    dat2name <- deparse(substitute(data2))
    colkeep <- colnames(data1)[colnames(data1)%in%colnames(data2)]
    colkeep <- colkeep[!colkeep%in%c("ActivityStartTime.TimeZoneCode", "ActivityStartTime.Time")]
    data1_1 <- data1[,colnames(data1)%in%colkeep]
    data1_1 <- unique(data1_1)
    data1_1$In_narrowresult <- "YES"
    data2_1 <- data2[,colnames(data2)%in%colkeep]
    data2_1 <- unique(data2_1)
    data2_1$In_access_file <- "YES"
    
    if(dat2name=="detquantlim"){
      print("NOTE: narrowresult will likely have many orphan records not represented in detquantlim. This occurs for a few reasons: (1) labs sometimes do not report detection quantitation limits, and (2) field measurements often do not report detection quantitation limits.")
      #readline(prompt="Press [enter] to continue")
    }
    
    if("ActivityStartDate"%in%colkeep){
      data1_1$ActivityStartDate <- as.Date(data1_1$ActivityStartDate)
      data2_1$ActivityStartDate <- as.Date(data2_1$ActivityStartDate)
    }
    
    orphmerge <- merge(data1_1, data2_1, all=TRUE)
    orphmerge1 <- orphmerge[is.na(orphmerge$In_narrowresult)|is.na(orphmerge$In_access_file),]
    
    # orphans in data1
    orph1 <- orphmerge[is.na(orphmerge$In_access_file),]
    if(length(orph1[,1])>0){
      print(paste0(length(orph1[,1])," orphan records detected in ", dat1name," file with no match to ", dat2name,"."))
      #readline(prompt="Press [enter] to continue")
    }
    
    # orphans in data2
    orph2 <- orphmerge[is.na(orphmerge$In_narrowresult),]
    if(length(orph2[,1])>0){
      print(paste0(length(orph2[,1])," orphan records detected in ", dat2name," file with no match to ", dat1name,"."))
      #readline(prompt="Press [enter] to continue")
    }
    if(length(orphmerge1[,1])>0){
      name <- paste(dat1name,dat2name,"orphans", sep="_")
      print(paste(name,"object created containing orphan records."))
    }
    return(orphmerge1)
  }
  
  # narrowresult and sites
  nr_site_orphans <- orph_check(narrowresult, sites)
  
  if(dim(nr_site_orphans)[1]>0){
    wqpdat$narrowresult_site_orphans <- nr_site_orphans
  }
  
  # narrowresult and activity
  print("Date forms between narrowresult and activity often cause erroneous orphans. Check date forms below. If date forms do not match, prior conversion using as.Date() is needed.")
  print("narrowresult file:")
  print(head(narrowresult$ActivityStartDate))
  print("activity file:")
  print(head(activity$ActivityStartDate))
  #readline(prompt="Press [enter] to continue")
  
  nr_act_orphans <- orph_check(narrowresult,activity)
  
  if(dim(nr_act_orphans)[1]>0){
    wqpdat$narrowresult_activity_orphans <- nr_act_orphans
  }
  
  # narrowresult and detquantlim
  nr_det_orphans <- orph_check(narrowresult,detquantlim)
  
  if(dim(nr_det_orphans)[1]>0){
    wqpdat$narrowresult_detquantlim_orphans <- nr_det_orphans
  }
}

## Check for non-numeric data in numeric columns ##

# Replace Common E.coli notation for ND and OD.
wqpdat$merged_results$ResultMeasureValue[wqpdat$merged_results$ResultMeasureValue=="<1" & wqpdat$merged_results$ResultMeasure.MeasureUnitCode=="MPN/100ml"] = 1
wqpdat$merged_results$ResultMeasureValue[wqpdat$merged_results$ResultMeasureValue==">2419.6" & wqpdat$merged_results$ResultMeasure.MeasureUnitCode=="MPN/100ml"] = 2420

print("Need to figure out non-numeric data in numeric columns conundrum.")
# if(num_check){
#   print("----NON-NUMERIC CHARACTERS IN NUMERIC COLUMN CHECK----")
#   
#   # Search for any non-numeric data in numeric columns
#   det_num <- which(is.na(suppressWarnings(as.numeric(as.character(detquantlim$DetectionQuantitationLimitMeasure.MeasureValue)))) & !is.na(detquantlim$DetectionQuantitationLimitMeasure.MeasureValue))
#   nr_num <- which(is.na(suppressWarnings(as.numeric(as.character(narrowresult$ResultMeasureValue)))) & !is.na(narrowresult$ResultMeasureValue))
#   
#   # Produces editable table that saves edits in new object added to list.
#   if(length(det_num)>0){
#   det_edits <- edit(detquantlim[det_num,names(detquantlim)%in%c("ActivityIdentifier","ResultIdentifier","CharacteristicName","DetectionQuantitationLimitTypeName","DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureUnitCode")])
#   detquantlim[det_num,detquantlim$DetectionQuantitationLimitMeasure.MeasureValue] = det_edits$DetectionQuantitationLimitMeasure.MeasureValue
#   }
#   
#   if(length(nr_num)>0){
#   nr_edits <- edit(narrowresult[nr_num,names(narrowresult)%in%c("ActivityIdentifier","ResultIdentifier","CharacteristicName","ResultMeasureValue","ResultMeasure.MeasureUnitCode")])
#   narrowresult[nr_num,narrowresult$ResultMeasureValue] = nr_edits$ResultMeasureValue
#   
#   }
# }

# Convert numeric columns to numeric
wqpdat$merged_results$ResultMeasureValue = wqTools::facToNum(wqpdat$merged_results$ResultMeasureValue)
wqpdat$detquantlim$DetectionQuantitationLimitMeasure.MeasureValue = wqTools::facToNum(wqpdat$detquantlim$DetectionQuantitationLimitMeasure.MeasureValue)

print("----FILES SUCCESSFULLY ADDED TO R OBJECT LIST----")
return(wqpdat)
}