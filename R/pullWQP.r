#' Pull water quality, station, and other data for Utah from USEPA to R global environment.
#'
#' Pull data from EPA's Water Quality Portal (WQP) as .csv directly into R environment for validation, screening, and assessment.
#' Prepared by C Shope 1/25/18, Adapted from Roop Guha-NJDEP 1/3/18. Updated, re-formated to function, & packaged by J VanderLaan.
#' @param StartDate Query start date. "MM-DD-YYYY" format.
#' @param EndDate Query end date. "MM-DD-YYYY" format.
#' @param retrieve Vector of data type names to retrieve from WQP. One or more of: "result","narrowresult","activity","activitymetric","sites","detquantlim". Defaults to query "sites", "narrowresult", "activity", and "detquantlim".
#' @param retry Logical value specifying whether function should continuously attempt a connection with USEPA WQP. Default set to TRUE.
#' @return Returns a list of objects of the selected data types over specified date range.
#' @export 

pullWQP<-function(StartDate,EndDate,retrieve=c("narrowresult","activity","sites","detquantlim"), retry=TRUE){

  out=list()
  
if(retry==TRUE){
    ## PULL THE STATION DATA TO R ENVIRONMENT (JV - I've found station pulls to work best straight to csv w/o zipping.)
    # Get the WQP station data with given date range
    if("sites" %in% retrieve){
      while(exists("sites")==FALSE){
        SiteSource=paste0("https://www.waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
        try(sites <- read.csv(SiteSource, stringsAsFactors = FALSE))
      }
      out$sites <- sites
      print(paste("Object 'sites' created. Consists of",dim(sites)[1],"rows and",dim(sites)[2],"columns."))
    }
        
    ## PULL THE WATER QUALITY DATA TO R ENVIRONMENT (wide and narrow result files)
    # Get the WQP result data with given date range
    if("result" %in% retrieve){
      while(exists("result")==FALSE){
        RSource=paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=", StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
        try(result <- read.csv(RSource, stringsAsFactors = FALSE))
      }
      out$result <- result
      print(paste("Object 'result' created. Consists of",dim(result)[1],"rows and",dim(result)[2],"columns."))
      }
    
    if("narrowresult" %in% retrieve){
      while(exists("narrowresult")==FALSE){
        NRSource=paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no&dataProfile=narrowResult")
        try(narrowresult <- read.csv(NRSource, stringsAsFactors = FALSE))
      }
      out$narrowresult <- narrowresult
      print(paste("Object 'narrowresult' created. Consists of",dim(narrowresult)[1],"rows and",dim(narrowresult)[2],"columns."))
      }
        
        
    ## PULL THE SAMPLING ACTIVITY DATA TO R ENVIRONMENT
    # Get the WQP sampling activities data with given date range
    if("activity" %in% retrieve){
      while(exists("activity")==FALSE){
        ActSource=paste0("https://www.waterqualitydata.us/Activity/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
        try(activity <- read.csv(ActSource, stringsAsFactors = FALSE))
      }
      out$activity <- activity
      print(paste("Object 'activity' created. Consists of",dim(activity)[1],"rows and",dim(activity)[2],"columns."))
      }
        
    ## PULL THE SAMPLING ACTIVITY METRICS DATA TO R ENVIRONMENT
    # Get the WQP sampling activity metrics data with given date range
    if("activitymetric" %in% retrieve){
     while(exists("activitymetric")==FALSE){
       ActMetSource=paste0("https://www.waterqualitydata.us/ActivityMetric/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
        try(activitymetric <- read.csv(ActMetSource, stringsAsFactors = FALSE))
      }
      out$activitymetric <- activitymetric
      print(paste("Object 'activitymetric' created. Consists of",dim(activitymetric)[1],"rows and",dim(activitymetric)[2],"columns."))
      }
  
    ## PULL THE RESULT QUANTITATION DATA TO R ENVIRONMENT
    # Get the WQP result quantitation data with given date range
    if("detquantlim" %in% retrieve){
      while(exists("detquantlim")==FALSE){
        DQLSource=paste0("https://www.waterqualitydata.us/ResultDetectionQuantitationLimit/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
        try(detquantlim <- read.csv(DQLSource, stringsAsFactors = FALSE))
        }
      out$detquantlim <- detquantlim
      print(paste("Object 'detquantlim' created. Consists of",dim(detquantlim)[1],"rows and",dim(detquantlim)[2],"columns."))
      }  
}else{
  ## PULL THE STATION DATA TO R ENVIRONMENT (JV - I've found station pulls to work best straight to csv w/o zipping.)
  # Get the WQP station data with given date range
  if("sites" %in% retrieve){
      SiteSource=paste0("https://www.waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
      try(sites <- read.csv(SiteSource, stringsAsFactors = FALSE))
      if(exists("sites")){
        out$sites <- sites
        print(paste("Object 'sites' created. Consists of",dim(sites)[1],"rows and",dim(sites)[2],"columns."))
        }else{print("Object 'sites' not created")}
    }
    
  ## PULL THE WATER QUALITY DATA TO R ENVIRONMENT (wide and narrow result files)
  # Get the WQP result data with given date range
  if("result" %in% retrieve){
      RSource=paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=", StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
      try(result <- read.csv(RSource, stringsAsFactors = FALSE))
      if(exists("result")){
        out$result <- result
        print(paste("Object 'result' created. Consists of",dim(result)[1],"rows and",dim(result)[2],"columns."))
        }else{print("Object 'result' not created")}
    }
  
  if("narrowresult" %in% retrieve){
      NRSource=paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no&dataProfile=narrowResult")
      try(narrowresult <- read.csv(NRSource, stringsAsFactors = FALSE))
      if(exists("narrowresult")){
        out$narrowresult <- narrowresult
        print(paste("Object 'narrowresult' created. Consists of",dim(narrowresult)[1],"rows and",dim(narrowresult)[2],"columns."))
        }else{print("Object 'narrowresult' not created")}
    }
  
  ## PULL THE SAMPLING ACTIVITY DATA TO R ENVIRONMENT
  # Get the WQP sampling activities data with given date range
  if("activity" %in% retrieve){
      ActSource=paste0("https://www.waterqualitydata.us/Activity/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
      try(activity <- read.csv(ActSource, stringsAsFactors = FALSE))
      if(exists("activity")){
        out$activity <- activity
        print(paste("Object 'activity' created. Consists of",dim(activity)[1],"rows and",dim(activity)[2],"columns."))
        }else{print("Object 'activity' not created")}
    }
  
  ## PULL THE SAMPLING ACTIVITY METRICS DATA TO R ENVIRONMENT
  # Get the WQP sampling activity metrics data with given date range
  if("activitymetric" %in% retrieve){
      ActMetSource=paste0("https://www.waterqualitydata.us/ActivityMetric/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
      try(activitymetric <- read.csv(ActMetSource, stringsAsFactors = FALSE))
      if(exists("activitymetric")){
        out$activitymetric <- activitymetric
        print(paste("Object 'activitymetric' created. Consists of",dim(activitymetric)[1],"rows and",dim(activitymetric)[2],"columns."))
        }else{print("Object 'activitymetric' not created")}
    }
    
  ## PULL THE RESULT QUANTITATION DATA TO R ENVIRONMENT
  # Get the WQP result quantitation data with given date range
  if("detquantlim" %in% retrieve){
      DQLSource=paste0("https://www.waterqualitydata.us/ResultDetectionQuantitationLimit/search?countrycode=US&statecode=US%3A49&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no")
      try(detquantlim <- read.csv(DQLSource, stringsAsFactors = FALSE))
      if(exists("detquantlim")){
        out$detquantlim <- detquantlim
        print(paste("Object 'detquantlim' created. Consists of",dim(detquantlim)[1],"rows and",dim(detquantlim)[2],"columns."))
        }else{print("Object 'detquantlim' not created")}
    }
  }
return(out)
}



