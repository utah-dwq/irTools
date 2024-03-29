#' Run HFDO assessments
#'
#' Performs high frequency dissolved oxygen assessments per IR assessment methods. This includes checking for data sufficiency, calculating daily minima and averages, and 7-day/30-day moving averages.
#'
#' @param data HFDO data--for the time being is a test dataset built by EH using "hfdo_prep.R".
#' @param min_n Minimum sample count for completing an assessment. This is used to generate the necessary number of consecutive days to calculate a moving average (averaging period length + min_n - 1).
#' @return List object containing data used for minimum, 7-day, and 30-day assessments, full list of assessments by type, and a rollup to site-use assessments.
#' @importFrom plyr ddply
#' @importFrom plyr rbind.fill
#' @importFrom tidyr unnest
#' @importFrom lubridate hour
#' @importFrom lubridate hm
#' @export assessHFDO

#### TESTING ####
# library(lubridate)
# library(plyr)
# load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
# data = hfdo_data
# data$DailyAggFun[data$AsmntAggPeriod>1]="mean" #These should be means in the standards table
# data$AsmntAggFun=data$DailyAggFun #These should be means in the standards table
# head(data)
# min_n=10
# HFDO_assessed=assessHFDO(data)
#save(file="F:\\Shiny\\hfdo\\data\\assessed_hfdo.Rdata", HFDO_assessed)

#JV note - There's a few more factors I'd like to carry through the process for later use/interpretation. But we'll have to do that when we have more "real" data.
	#Units (IR & Criterion), site-specific info (descriptions, MLID, & time periods), maybe site/AU names (but these could also be stitched back later).
	#We could specify our grouping keys in ddply in reverse if it's easier (include everything except date, time, etc as appropriate).
 
assessHFDO <- function(data, min_n=10){
  
  HFDO_assessed <- list()
 
  data$time=as.Date(lubridate::hms(data$ActivityStartTime.Time), origin=lubridate::origin)
  data$hour=lubridate::hour(data$time)
  head(data)
  data$NumericCriterion = as.numeric(data$NumericCriterion) #For some reason criteria were data type "character"
  
  data=droplevels(data)

  agg_full_days <- function(x){ # Subsets data to complete days and calculates daily mins/means for complete days only
    # Count samples per hr+date
    date_hour_count=aggregate(IR_Value~hour+ActivityStartDate, x, FUN="length", drop=F)
    names(date_hour_count)[names(date_hour_count)=="IR_Value"]="SampleCount"
    # table(date_hour_count$SampleCount, exclude=NULL)
    # head(date_hour_count[is.na(date_hour_count$SampleCount),])
    # head(date_hour_count[!is.na(date_hour_count$SampleCount),])
    
    date_hour_count=within(date_hour_count,{
      complete_hour=SampleCount
      complete_hour[is.na(complete_hour)]=0
      complete_hour[complete_hour>=1]=1
    })
    # head(date_hour_count[is.na(date_hour_count$SampleCount),])
    # head(date_hour_count[!is.na(date_hour_count$SampleCount),])
    
    # Count hours w/ sample per date
    hour_count=aggregate(complete_hour~ActivityStartDate, date_hour_count, FUN='sum')
    names(hour_count)[names(hour_count)=="complete_hour"]="complete_hours"
    head(hour_count)
    
    # ID complete days (those w/ 24 complete_hours)
    complete_days=hour_count[hour_count$complete_hours==24,]
    complete_days=complete_days[order(complete_days$ActivityStartDate),]
    complete_days$ActivityStartDate = as.character(complete_days$ActivityStartDate)
   
    #Subset data to complete days
    x$ActivityStartDate = as.character(x$ActivityStartDate)
    y=merge(complete_days, x, all.x = TRUE)
    
    #Generate daily means or mins
    daily_aggs=aggregate(IR_Value~ActivityStartDate, y, FUN=x$DailyAggFun[1])
    daily_aggs$ActivityStartDate = as.Date(daily_aggs$ActivityStartDate, format = "%Y-%m-%d")
    return(daily_aggs)
  }
  
  # Aggregate to daily means/mins for complete days of MLID/Use/Assessment Type....
  daily_values <- plyr::ddply(.data=data,c("IR_MLID","BEN_CLASS","ASSESS_ID", "ss_R317Descrp","BeneficialUse","R3172ParameterName","IR_Unit", "DailyAggFun", "AsmntAggPeriod","AsmntAggFun", "AsmntAggPeriodUnit","NumericCriterion", "CriterionUnits","ParameterQualifier"),.fun=agg_full_days)
  
  # Split off daily mins
  daily_values_min <- daily_values[daily_values$AsmntAggPeriod==1,]
  
  ## Assess daily minima
  min.do <- function(x){
    out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse","BEN_CLASS","ASSESS_ID","ss_R317Descrp","ParameterQualifier","NumericCriterion","AsmntAggPeriod","AsmntAggFun","IR_Unit","CriterionUnits","AsmntAggPeriodUnit")]
    if(length(x$ActivityStartDate)<min_n){
      out$SampleCount = length(x$ActivityStartDate)
      out$ExcCount = NA
      out$IR_Cat = "Not assessed: insufficient data"
    }else{
      x$Exc = ifelse(x$IR_Value<x$NumericCriterion,1,0)
      out$Exc = sum(x$Exc)
      out$SampleCount = dim(x)[1]
      out$Min_Date <- min(x$ActivityStartDate)
      out$Max_Date <- max(x$ActivityStartDate)
      out$ExcPerc = out$Exc/out$SampleCount
      out$IR_Cat = ifelse(out$ExcPerc>0.1,"NS","FS")
      
    }
    return(out) 
  }
  
  min_do_assessed <- plyr::ddply(.data=daily_values_min, c("IR_MLID","BEN_CLASS", "ASSESS_ID", "ss_R317Descrp","BeneficialUse","R3172ParameterName","ParameterQualifier"), .fun=min.do)
  
  # Moving window (7- and 30-day) assessments - determine which data fit adequate spacing requirement
  # EH NOTE: Have "MinContig" as column in standards table to specify spacing? Or could create input for it. Many options
  # JV NOTE: I think I'd like to set a min sample count via argument that applies to all assessments (default=10, then below add min_n arg - 1 to generate MinContig)
  # <ADDED FOR THIS DRAFT>
  
  daily_values_mean <- daily_values[!daily_values$AsmntAggPeriod==1,]
  
  # EH: No longer need min contig here
  # daily_values_mean$MinContig = daily_values_mean$AsmntAggPeriod +  min_n - 1
  
  # testing adeq_space
  # x = daily_values_mean[daily_values_mean$IR_MLID=="UTAHDWQ_WQX-4991900"&daily_values_mean$BeneficialUse=="3B"&daily_values_mean$MinContig==16,]
  # unique(x$NumericCriterion)
    
  adeq_space <- function(x){
    
    # Order by date
    x$ActivityStartDate = as.Date(x$ActivityStartDate, format = "%Y-%m-%d")
    x = x[order(x$ActivityStartDate),]
    agg_period = x$AsmntAggPeriod[1]
    
    # ID groups of complete days
    x$diff = c(1,diff(x$ActivityStartDate)) # determine difference between value and the one before it--starts with number just greater than zero to ensure first record kept.
    x$diff_log <- x$diff==1 # create logical vector to "flag" when difference greater than the proportion of 1 day between two values 
    rles <- rle(x$diff_log) # count consecutive numbers of TRUE and FALSE values
    groups <- data.frame(rles$lengths, rles$values)
    
    # Find rows where groups start and stop (e.g. where spacing becomes > 1 day)
    groups <- within(groups,{
      index_upper = cumsum(rles$lengths) # determine where consecutive pattern stops
      index_lwr = index_upper-rles$lengths # determine where consecutive pattern starts (starts on FALSE value, which signifies that value is more than 1 day away from the sample value before it)
    })
    groups$index_lwr[groups$index_lwr==0]=1 # zero will throw off function because there is no zero position in data frame
    groups$rles.lengths = groups$rles.lengths + 1
    groups$rles.lengths[1] = groups$rles.lengths[1] - 1
    
    groups$min_dat = x$ActivityStartDate[groups$index_lwr] 
    groups$max_dat = x$ActivityStartDate[groups$index_upper] 

    # Subset to ranges that fit the minimum consecutive days
    groups_fit = groups[groups$rles.lengths>=agg_period,]
    
    # Count number of rolling consecutive day averages
    num_rolling = sum(groups_fit$rles.lengths-agg_period+1)
    
    # Obtain data from original dataset falling within that date range
    # EH NOTE: There might be a way to accurately do this with the grouping factor scheme, but I could not figure it out. I was getting incorrect groupings using the test code.
    # I do not believe this for loop (nestled so deeply within ddply subsets) will take up much computing power. Let me know what you think. 
	# JV - I think this will be fine - it's lightning fast on the test data - it takes longer to compile the function than run the ddply lol.
    if(num_rolling>min_n){
      dat_list = list()
      for(i in 1:dim(groups_fit)[1]){
        chunk <- x$ActivityStartDate>=groups_fit$min_dat[i]&x$ActivityStartDate<=groups_fit$max_dat[i]
        x1 = x[chunk==TRUE,]
        #x1$group = as.numeric(x1$ActivityStartDate[1])
        dat_list[[i]] <- x1
      }
      out = do.call(rbind, dat_list)
      return(out)
    }else{return(NULL)}    
    
  }
 
adeq_space_values <- plyr::ddply(.data=daily_values_mean, c("IR_MLID", "BeneficialUse", "ss_R317Descrp","IR_Unit", "CriterionUnits", "AsmntAggPeriod", "AsmntAggPeriodUnit","AsmntAggFun","ParameterQualifier"), .fun=adeq_space)

# Moving window assessments function - 7 and 30 day 
movingwindow_assess <- function(x){
  out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse","BEN_CLASS","ASSESS_ID","ss_R317Descrp","AsmntAggPeriod", "ParameterQualifier")]
  out$Min_Date <- min(x$ActivityStartDate)
  out$Max_Date <- max(x$ActivityStartDate)
  datmean = c()
  numcrit = c()
  startdat = c()
  exceed = c()
  days = x$AsmntAggPeriod[1]-1
  m = 1
  for(i in 1:dim(x)[1]){
    dmin = x$ActivityStartDate[i]
    dmax = x$ActivityStartDate[i]+days
    datrange <- x[x$ActivityStartDate>=dmin&x$ActivityStartDate<=dmax,]
    if(dim(datrange)[1]< x$AsmntAggPeriod[1]){next}
    
    # Keep track of each comparison 
    numcrit[m] <- median(datrange$NumericCriterion) # Use median numeric criterion for each multi-day mean DO
    datmean[m] <- mean(datrange$IR_Value)
    exceed[m] <- ifelse(mean(datrange$IR_Value)<median(datrange$NumericCriterion),1,0)
    startdat[m] <- dmin
    m = m+1
  }
  out$means=list(datmean)
  out$dates=list(startdat)
  out$exceed = list(exceed)
  out$NumericCriterion = list(numcrit)
  out$SampleCount = length(datmean)
  out$Exc = sum(exceed)
  out$ExcPerc = sum(exceed)/length(datmean)
  out$IR_Cat = ifelse(out$ExcPerc>0.1,"NS","FS")
  return(out)
}
# EH note: removed "group" as a variable. I do not think it's needed anymore.
#thirty_seven_assessed <- plyr::ddply(.data=adeq_space_values, c("IR_MLID", "BeneficialUse", "IR_Unit", "NumericCriterion", "CriterionUnits", "AsmntAggPeriod", "AsmntAggPeriodUnit", "group","AsmntAggFun"), .fun=movingwindow_assess)
thirty_seven_assessed <- plyr::ddply(.data=adeq_space_values, c("IR_MLID", "BeneficialUse", "ss_R317Descrp","IR_Unit", "CriterionUnits", "AsmntAggPeriod", "AsmntAggPeriodUnit","AsmntAggFun","ParameterQualifier"), .fun=movingwindow_assess)

#Extract 30 & 7 d means
thirty_seven_means=tidyr::unnest(thirty_seven_assessed)
names(thirty_seven_means)[names(thirty_seven_means)=="means"]="mean"
names(thirty_seven_means)[names(thirty_seven_means)=="dates"]="start_date"
thirty_seven_means$start_date = as.Date(thirty_seven_means$start_date, origin = "1970-01-01")

#Drop extra cols
thirty_seven_assessed=thirty_seven_assessed[,!names(thirty_seven_assessed) %in% c("means","dates","group")]

### COMBINE ASSESSMENTS ###
allHFDO_asmnts = plyr::rbind.fill(min_do_assessed,thirty_seven_assessed)

HFDO_assessed$allHFDO_asmnts = allHFDO_asmnts

### Roll up to site-use assessment ###
allHFDO_asmnts.list <- list(allHFDO_asmnts[,!names(allHFDO_asmnts)%in%c("exceed","NumericCriterion")])
site_use_rollup = irTools::rollUp(allHFDO_asmnts.list,group_vars = c("IR_MLID","BeneficialUse","R3172ParameterName"),expand_uses=TRUE,print=TRUE)
HFDO_assessed$site_use_rollup = site_use_rollup

#Value added outputs (potentially for use in visualization tools):
HFDO_assessed$data=data
HFDO_assessed$daily_values=daily_values
HFDO_assessed$thirty_seven_means=thirty_seven_means

#save(file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\assessments\\HFDO_assessed_example.Rdata", HFDO_assessed)

return(HFDO_assessed)
}

