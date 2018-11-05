#' WQP Data Screen
#' 
#' Performs duplicate and orphan checks on data downloaded from WQP.
#' Alerts user to potential issues with running data through subsequent irTool functions.
#' 
#' @param outfile_path Path to folder where user wishes to deposit orphan check .csv's
#' @param sites_file Full path and filename of sites file queried from WQP to be reviewed (.csv).
#' @param activity_file Full path and filename of activity file queried from WQP to be reviewed (.csv).
#' @param narrowresult_file Full path and filename of narrowresult file queried from WQP to be reviewed (.csv).
#' @param detquantlim_file Full path and filename of detquantlim file queried from WQP to be reviewed (.csv).
#' @param dupcheck Vector of file names to be run through duplicate check. Allows user to save time if they already know whether some files do/do not contain duplicates.
#' 

## Testing Function ##

# WQPdataqaqc("C:\\Users\\ehinman\\Desktop",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\narrowresult981001-020930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\sites981001-020930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\activity981001-020930.csv",
#             "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\detquantlim981001-020930.csv")

WQPdataqaqc <- function(outfile_path, narrowresult_file, sites_file, activity_file, detquantlim_file, dupcheck=c("narrowresult","sites","activity","detquantlim")){
  
## Testing setup ##
# outfile_path = "C:\\Users\\ehinman\\Desktop"
# sites_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\sites101001-110930.csv"
# narrowresult_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\narrowresult101001-110930.csv"
# activity_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\activity101001-110930.csv"
# detquantlim_file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\01raw_data\\detquantlim101001-110930.csv"

print("------------READING IN FILES--------------")
narrowresult <- read.csv(narrowresult_file, stringsAsFactors = FALSE)#, strip.white = TRUE)
sites <- read.csv(sites_file, stringsAsFactors = FALSE)#, strip.white = TRUE)
activity <- read.csv(activity_file, stringsAsFactors = FALSE)#, strip.white = TRUE)
detquantlim <- read.csv(detquantlim_file, stringsAsFactors = FALSE)#, strip.white = TRUE)
  
### Check for duplicates ###
print("----PERFORMING EXACT DUPLICATE CHECKS-----")
dupes <- function(dataset){
  datname <- deparse(substitute(dataset))
  numdupes <- length(duplicated(dataset)[duplicated(dataset)==TRUE])
  if(numdupes>0){
    print(paste(datname,"file contains",numdupes,"exact duplicate(s). Review data before proceeding."))
    print(dataset[duplicated(dataset),])}else{print(paste("No exact duplicates in",datname,"file detected."))}
  }
if("narrowresult"%in%dupcheck){
  dupes(narrowresult)
}
if("sites"%in%dupcheck){
  dupes(sites)
}
if("activity"%in%dupcheck){
  dupes(activity)
}
if("detquantlim"%in%dupcheck){
  dupes(detquantlim)
}

### Check for orphans ###
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
    readline(prompt="Press [enter] to continue")
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
    readline(prompt="Press [enter] to continue")
  }

  # orphans in data2
  orph2 <- orphmerge[is.na(orphmerge$In_narrowresult),]
  if(length(orph2[,1])>0){
    print(paste0(length(orph2[,1])," orphan records detected in ", dat2name," file with no match to ", dat1name,"."))
    readline(prompt="Press [enter] to continue")
  }
  if(length(orphmerge1[,1])>0){
    name <- paste0(outfile_path,"\\",paste(dat1name,dat2name,"orphans", sep="_"),".csv")
    print(paste(name,"created containing orphan records."))
    write.csv(orphmerge1,name, row.names = FALSE)
  }
}

# narrowresult and sites
orph_check(narrowresult, sites)

# narrowresult and activity
print("Date forms between narrowresult and activity often cause erroneous orphans. Check date forms below. If date forms do not match, prior conversion using as.Date() is needed.")
print("narrowresult file:")
print(head(narrowresult$ActivityStartDate))
print("activity file:")
print(head(activity$ActivityStartDate))
readline(prompt="Press [enter] to continue")

orph_check(narrowresult,activity)

# narrowresult and detquantlim
orph_check(narrowresult,detquantlim)

}

