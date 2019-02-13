#' Assess E.coli data at the year/site level.
#'
#' Compares E.coli data to 30-day and max criterion standards using Scenarios A, B, and C, and assigns e. coli assessment categories to each site.
#'
#' @param data A prepped dataframe object (likely the ecoli object within the prepped_data list--prepped_data$ecoli) containing e.coli data at the site/use/parameter level with standards assigned. 
#' @param SeasonStartDate A string in the form "mm-dd" to define beginning of rec season over which to perform assessments.
#' @param SeasonEndDate A string in the form "mm-dd" to define end of rec season over which to perform assessments.
#' @param rec_season Logical. If TRUE, restricts assessments to recreation season data.
#' @return Returns list with three objects: assessments from all Scenarios on all data, and ecoli assessments aggregated over scenario and year and rolled up to site level.
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom plyr ddply
#' @importFrom dplyr bind_rows
#' @export

## TESTING ###
# data <- read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\e.coli_demo\\01_rawdata\\ecoli_example_data.csv")
# SeasonStartDate="05-01"
# SeasonEndDate="10-31"
# rec_season = TRUE
# 
# library(irTools)
# ecoli_data=read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\e.coli_demo\\01_rawdata\\ecoli_example_data.csv")
# table(ecoli_data$IR_MLID)
# #
# asmnt=assessEColi(ecoli_data)
# objects(asmnt)
# table(asmnt$ecoli_mlid_asmnts$IR_MLID)


assessEColi <- function(data, rec_season = TRUE, SeasonStartDate="05-01", SeasonEndDate="10-31"){
 
  # Create new object for holding ecoli assessments
  ecoli_assessments = list()
  
  # Read in data
  data_raw = data
  
  # Geometric mean function
  gmean <- function(x){exp(mean(log(x)))}
  
  # Obtain unique use/criterion 
  uses_stds <- unique(data_raw[c("BeneficialUse","CriterionLabel","NumericCriterion")])
  
  # Remove duplicates from data
  data_raw <- data_raw[,!names(data_raw)%in%c("AsmntAggPeriod","AsmntAggPeriodUnit","AsmntAggFun","CriterionLabel","NumericCriterion")]
  data_raw <- unique(data_raw)
  
  # Convert dates to R dates
  data_raw$ActivityStartDate=as.Date(data_raw$ActivityStartDate,format='%m/%d/%Y')
  
  # Create year column for scenario calculations
  data_raw$Year=lubridate::year(data_raw$ActivityStartDate)
  
  # Restrict assessments to data collected during recreation season.
  if(rec_season){
    data_raw$Rec_Season=ifelse(lubridate::month(data_raw$ActivityStartDate)>=lubridate::month(as.Date(SeasonStartDate,format='%m-%d'))
                      &lubridate::day(data_raw$ActivityStartDate)>=lubridate::day(as.Date(SeasonStartDate,format='%m-%d'))
                      &lubridate::month(data_raw$ActivityStartDate)<=lubridate::month(as.Date(SeasonEndDate,format='%m-%d'))
                      &lubridate::day(data_raw$ActivityStartDate)<=lubridate::day(as.Date(SeasonEndDate,format='%m-%d')),"Yes","No")
    
    # Separate rec and non-rec season samples
    data_rec <- data_raw[data_raw$Rec_Season=="Yes",]
    data_nonrec <- data_raw[data_raw$Rec_Season=="No",]
    
    # Determine which non-rec season MLIDs/Use/Years are unique from rec season MLIDs/Use/Years
    rec_mlids <- unique(data_rec[,c("IR_MLID","BeneficialUse","Year","Rec_Season")])
    nonrec_mlids <- unique(data_nonrec[,c("IR_MLID","BeneficialUse","Year")])
    nonrec_uni <- merge(rec_mlids, nonrec_mlids, all=TRUE)
    nonrec_unique <- nonrec_uni[is.na(nonrec_uni$Rec_Season),c("IR_MLID","BeneficialUse","Year")]
    nonrec_unique$IR_Cat <- "Not Assessed - Out of Rec Season"
    nonrec_data <- merge(nonrec_unique,data_raw, all.x=TRUE)
    ecoli_assessments$non_assessed_data <- nonrec_data
  }else{
      data_rec = data_raw
    }
  
  # Aggregate to daily values.
  # data_raw$IR_Value=gsub("<1",1,data_raw$IR_Value)
  # data_raw$IR_Value=as.numeric(gsub(">2419.6",2420,data_raw$IR_Value))
  daily_agg=aggregate(IR_Value~IR_MLID+BeneficialUse+ActivityStartDate,data=data_rec,FUN=function(x){exp(mean(log(x)))})
  data_processed <- merge(daily_agg,unique(data_rec[,!names(data_rec)%in%c("ActivityStartTime.Time")]), all.x=TRUE)

  # JV NOTE - <1 & >2419.6 are coerced to numeric in fillMaskedValues. Simplest solution I can think of right now is to do the conversions in 65 & 66 before coercing to numeric in fillMaskedValues.
  # Should we manually set anything over 2420 to 2420 for assessment? Some over-detect results in WQP have higher limits reported.
  
  # maxSamps48hr function - counts the maximum number of samples collected over the rec season that were not collected within 48 hours of another sample(s).
  maxSamps48hr = function(x){
    x = sort(x) # order by DOY
    consecutive.groupings <- c(0, which(diff(x) != 1), length(x)) # Determine breaks in 1 by 1 sequence
    consec.groups <- sum(ceiling(diff(consecutive.groupings)/2)) # Determine length of each sequential group, divide by two, and round up to get the max number of samples occurring at least 48 hours apart
    return(consec.groups)
  }

##### SCENARIO A #####
  
  assessA = function(x){
    out_48_hr = maxSamps48hr(x$ActivityStartDate)
    stdcrit = uses_stds$NumericCriterion[uses_stds$BeneficialUse==x$BeneficialUse[1]&uses_stds$CriterionLabel=="max_crit"]
    out <- x[1,c("IR_MLID","BeneficialUse","Year")]
    out$Scenario = "A"
    out$SampleCount = length(x$ActivityStartDate)
    out$ExcCountLim = ifelse(out_48_hr>=5,ceiling(out_48_hr*.1),1)
    out$ExcCount = length(x$IR_Value[x$IR_Value>stdcrit])
    if(out$SampleCount<5){
      out$IR_Cat = ifelse(out$ExcCount>out$ExcCountLim,"idE","idNE")
    }else{
      out$IR_Cat = ifelse(out$ExcCount>out$ExcCountLim,"NS","ScenB")
    }
    return(out)
    }

##### SCENARIO B #####

  assessB <- function(x){
    out_48_hr = maxSamps48hr(x$ActivityStartDate)
    if(out_48_hr<5){
      out <- x[1,c("IR_MLID","BeneficialUse","Year")]
      out$SampleCount = out_48_hr
      out$IR_Cat = "Not Assessed"
      out$Scenario = "B"
    }
    stdcrit = uses_stds$NumericCriterion[uses_stds$BeneficialUse==x$BeneficialUse[1]&uses_stds$CriterionLabel=="30-day"]
    # Create empty vector to hold geometric means
    geomeans <- vector()
    n = 1 # counter for geomeans vector population
    # Loop through each day 
    for(i in 1:dim(x)[1]){
      dmax = x$ActivityStartDate[i]+29 # create 30 day window
      samps = x[x$ActivityStartDate>=x$ActivityStartDate[i]&x$ActivityStartDate<=dmax,] # Isolate samples occurring within that window
      # Calculate geometric mean on ALL samples IF 5 or more samples are spaced at least 48 hours apart
      geomeans[i] <- ifelse(maxSamps48hr(samps$ActivityStartDate)>=5,gmean(samps$IR_Value),0)
      }
    # Create output dataframe with MLID/Use/Year/SampleCount...etc.
      out <- samps[1,c("IR_MLID","BeneficialUse","Year")]
      out$SampleCount = length(geomeans)
      out$ExcCountLim = 1
      out$ExcCount = length(geomeans[geomeans>=stdcrit])
      out$IR_Cat = ifelse(any(geomeans>=stdcrit),"NS","ScenC")
      out$Scenario = "B"
      return(out)
  }

##### SCENARIO C #####
  
  assessC <- function(x){
    out_48_hr = maxSamps48hr(x$ActivityStartDate)
    if(out_48_hr<5){
      out <- x[1,c("IR_MLID","BeneficialUse","Year")]
      out$SampleCount = out_48_hr
      out$IR_Cat = "Not Assessed"
      out$Scenario = "C"
    }else{
      stdcrit = uses_stds$NumericCriterion[uses_stds$BeneficialUse==x$BeneficialUse[1]&uses_stds$CriterionLabel=="30-day"]
      out <- x[1,c("IR_MLID","BeneficialUse","Year")]
      out$Scenario = "C"
      out$SampleCount = length(x$ActivityStartDate)
      geomean <- gmean(x$IR_Value)
      if(out$SampleCount<10){
        out$IR_Cat = ifelse(geomean>stdcrit,"idE","idNE")
      }else{
        out$IR_Cat = ifelse(geomean>stdcrit,"NS","FS")
      }
    }
    return(out)
  }
  
    ##### COMBINE SCENARIOS ####
  
    ScenA = plyr::ddply(.data=data_processed,c("IR_MLID","BeneficialUse","Year"),.fun=assessA) #.() was not working when installed as package w/o importing all of plyr
    ScenB = plyr::ddply(.data=data_processed,c("IR_MLID","BeneficialUse","Year"),.fun=assessB)
    ScenC = plyr::ddply(.data=data_processed,c("IR_MLID","BeneficialUse","Year"),.fun=assessC)

    # Create object with all data run through all assessment scenarios
    ecoli_assessments$ecoli_scenario_asmnts = dplyr::bind_rows(ScenA,ScenB,ScenC, nonrec_unique)

### Rank Scenarios ###
  
  ScenABC = dplyr::bind_rows(ScenA, ScenB, ScenC)
  ScenABC = ScenABC[!(ScenABC$IR_Cat=="ScenB"|ScenABC$IR_Cat=="ScenC"|ScenABC$IR_Cat=="Not Assessed"),]
  ScenABC$S.rank = 3
  ScenABC$S.rank[ScenABC$Scenario=="B"] = 2
  ScenABC$S.rank[ScenABC$Scenario=="C"] = 1
  
### Roll up by scenario ###
  
  ScenABC_agg <- aggregate(S.rank~IR_MLID+BeneficialUse+Year, data=ScenABC, FUN=max)
  ScenABC_agg$Scenario = "A"
  ScenABC_agg$Scenario[ScenABC_agg$S.rank==2]="B"
  ScenABC_agg$Scenario[ScenABC_agg$S.rank==1]="C"
  ScenABC_assess <- merge(ScenABC_agg, ScenABC, all.x=TRUE)
  ScenABC_assess = ScenABC_assess[,!names(ScenABC_assess)%in%c("S.rank")]
  ScenABC_assess = dplyr::bind_rows(ScenABC_assess, nonrec_unique)
  
  ecoli_assessments$ecoli_scenario_rollup = ScenABC_assess

### Roll up to site level assessments ###
  
  # Merge data back to original dataset
  data_raw1 <- unique(data_raw[,c("IR_MLID","ASSESS_ID","BeneficialUse","BEN_CLASS","R3172ParameterName")])
  
  ecoli.assessed <- merge(ecoli_assessments$ecoli_scenario_asmnts,data_raw1, all.x=TRUE)
  ecoli.assessed <- ecoli.assessed[ecoli.assessed$IR_Cat=="NS"|
                                    ecoli.assessed$IR_Cat=="idNE"|
                                    ecoli.assessed$IR_Cat=="idE"|
                                     ecoli.assessed$IR_Cat=="FS",]
  
  # Represent categories numerically so we can select the "max" category to define the AU
  # Hierarchy of decision making within each subset: NS>TMDLa>idE>FS>idNE
  ecoli.assessed$AssessCat[ecoli.assessed$IR_Cat=="NS"]<-5
  ecoli.assessed$AssessCat[ecoli.assessed$IR_Cat=="idE"]<-3
  ecoli.assessed$AssessCat[ecoli.assessed$IR_Cat=="FS"]<-2
  ecoli.assessed$AssessCat[ecoli.assessed$IR_Cat=="idNE"]<-1
  
  ecoli.assess.agg <- aggregate(AssessCat~IR_MLID+ASSESS_ID+BeneficialUse+BEN_CLASS+R3172ParameterName, data=ecoli.assessed, FUN=max)
  
  names(ecoli.assess.agg)[names(ecoli.assess.agg)=="AssessCat"]<- "IR_Cat"
  
  #Renaming assessment categories
  ecoli.assess.agg$IR_Cat=as.character(ecoli.assess.agg$IR_Cat)
  ecoli.assess.agg=within(ecoli.assess.agg,{
    IR_Cat[IR_Cat=="5"]="NS"
    IR_Cat[IR_Cat=="3"]="idE"
    IR_Cat[IR_Cat=="2"]="FS"
    IR_Cat[IR_Cat=="1"]="idNE"
  })
  
  ecoli_assessments$ecoli_mlid_asmnts = ecoli.assess.agg
  
  return(ecoli_assessments)
} 
