#' Assess E.coli data at the year/site level.
#'
#' Compares E.coli data to 30-day and max criterion standards using Scenarios A, B, and C, and assigns e. coli assessment categories to each site.
#'
#' @param data A prepped dataframe object (likely the ecoli object within the prepped_data list--prepped_data$ecoli) containing e.coli data at the site/use/parameter level with standards assigned. 
#' @param SeasonStartDate A string in the form "mm-dd" to define beginning of rec season over which to perform assessments.
#' @param SeasonEndDate A string in the form "mm-dd" to define end of rec season over which to perform assessments.
#' @param rec_season Logical. If TRUE, restricts assessments to recreation season data.
#' @param max_exc_pct Maximum allowable exceedance percentage for full support (exceedance pcts > max_exc_pct are considered not supporting - one of max_exc_count or max_exc_pct must be specified
#' @return Returns list with three objects: assessments from all Scenarios on all data, and ecoli assessments aggregated over scenario and year and rolled up to site level.
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom plyr ddply
#' @import dplyr
#' @import tidyr
#' @export

## TESTING ###
# library(irTools)
# ecoli_data=read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\z_archives\\e.coli_demo\\01_rawdata\\ecoli_example_data.csv")
# table(ecoli_data$IR_MLID)
# asmnt_rec=assessEColi(ecoli_data)
# recmlids <- as.character(unique(asmnt_rec$ecoli_allscenario_asmnts$IR_MLID))
# nonrecmlids <- as.character(unique(asmnt_rec$non_assessed_data$IR_MLID))
# unique_mlids1 <- unique(c(recmlids,nonrecmlids))
# 
# asmnt_nonrec= assessEColi(ecoli_data, rec_season = FALSE)
# unique_mlids2 <- as.character(unique(asmnt_nonrec$ecoli_allscenario_asmnts$IR_MLID))
# 
# test1 <- unique_mlids1[!unique_mlids1%in%unique_mlids2] # Empty
# test2 <- unique_mlids2[!unique_mlids2%in%unique_mlids1] # Empty


assessEColi <- function(data, rec_season = TRUE, SeasonStartDate="05-01", SeasonEndDate="10-31", max_exc_pct){
 
  # data <- read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\e.coli_demo\\01_rawdata\\ecoli_example_data.csv")
  # SeasonStartDate="05-01"
  # SeasonEndDate="10-31"
  # rec_season = TRUE
  
  # Create new object for holding ecoli assessments
  ecoli_assessments = list()
  
  # Read in data
  data_raw = data
  
  ecoli_assessments$raw_data = data
  
  # Geometric mean function
  gmean <- function(x){exp(mean(log(x)))}
  
  # Obtain unique use/criterion 
  uses_stds <- unique(data_raw[,c("BeneficialUse","CriterionLabel","NumericCriterion")])
  uses_stds$NumericCriterion=as.numeric(uses_stds$NumericCriterion)
  
  # Remove duplicates from data
  data_raw <- data_raw[,!names(data_raw)%in%c("AsmntAggPeriod","AsmntAggPeriodUnit","AsmntAggFun","CriterionLabel","NumericCriterion")]
  data_raw <- unique(data_raw)
  
  # Convert dates to R dates
  data_raw$ActivityStartDate=as.Date(data_raw$ActivityStartDate,format='%Y-%m-%d')
  
  # Create year column for scenario calculations
  data_raw$Year=lubridate::year(data_raw$ActivityStartDate)
  
  # JV NOTE - <1 & >2419.6 are coerced to numeric in fillMaskedValues. Simplest solution I can think of right now is to do the conversions in 65 & 66 before coercing to numeric in fillMaskedValues.
  # EH NOTE: <1 and >2419.6 now handled in readWQP data, before data are coerced to numeric. However, retained in this assessment tool for (potential) data that bypass other segments of irTools.
  # Should we manually set anything over 2420 to 2420 for assessment? Some over-detect results in WQP have higher limits reported.
  data_raw$IR_Value=gsub("<1",1,data_raw$IR_Value)
  data_raw$IR_Value=as.numeric(gsub(">2419.6",2420,data_raw$IR_Value))
  
  # Restrict assessments to data collected during recreation season.
  if(rec_season){
    data_raw$Rec_Season=ifelse(lubridate::month(data_raw$ActivityStartDate)>=lubridate::month(as.Date(SeasonStartDate,format='%m-%d'))
                      &lubridate::day(data_raw$ActivityStartDate)>=lubridate::day(as.Date(SeasonStartDate,format='%m-%d'))
                      &lubridate::month(data_raw$ActivityStartDate)<=lubridate::month(as.Date(SeasonEndDate,format='%m-%d'))
                      &lubridate::day(data_raw$ActivityStartDate)<=lubridate::day(as.Date(SeasonEndDate,format='%m-%d')),"Yes","No")
    
    # Separate rec and non-rec season samples
    data_rec <- data_raw[data_raw$Rec_Season=="Yes",]
    data_nonrec <- data_raw[data_raw$Rec_Season=="No",]
    if(dim(data_nonrec)[1]>0){
      data_nonrec$IR_Cat <- "Not Assessed - Out of Rec Season"
    }
    
    # Place non-rec season samples in own REJECT object
    ecoli_assessments$non_assessed_data <- data_nonrec
    }else{
      data_rec = data_raw
    }
  
  ecoli_assessments$assessed_data = data_rec
  
  # Aggregate to daily values.
  data_processed=aggregate(IR_Value~IR_MLID+BeneficialUse+ActivityStartDate+Year,data=data_rec,FUN=function(x){exp(mean(log(x)))})
  
  cols2keep = c("ActivityStartDate","IR_MLID","IR_MLNAME","ASSESS_ID","AU_NAME","AU_Type","BEN_CLASS","R3172ParameterName",
                "IR_Value","IR_Unit","IR_DetCond","IR_Fraction","IR_ActivityType","IR_Lat","IR_Long","DataLoggerLine",
                "ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","R317Descrp","ActivityDepthHeightMeasure.MeasureUnitCode")
  
  data_processed_allcols <- merge(data_processed,unique(data_rec[,cols2keep]), all.x=TRUE)
  ecoli_assessments$dailyaggregated_data = data_processed

  # maxSamps48hr function - counts the maximum number of samples collected over the rec season that were not collected within 48 hours of another sample(s).
  maxSamps48hr = function(x){
    x = sort(x) # order by DOY
    consecutive.groupings <- c(0, which(diff(x) != 1), length(x)) # Determine breaks in 1 by 1 sequence
    consec.groups <- sum(ceiling(diff(consecutive.groupings)/2)) # Determine length of each sequential group, divide by two, and round up to get the max number of samples occurring at least 48 hours apart
    return(consec.groups)
  }
  
  # Rounding function to ensure if 10% of the sample count is x.5 or higher, it will round up, and if it's x.4 or lower, it will round down.
  rounding = function(x,max_exc_pct){
    y = x*max_exc_pct/100+0.5
    z = floor(y)
    return(z)
  }

##### SCENARIO A #####
  
  assessA = function(x){
    out_48_hr = maxSamps48hr(x$ActivityStartDate)
    stdcrit = uses_stds$NumericCriterion[uses_stds$BeneficialUse==x$BeneficialUse[1]&uses_stds$CriterionLabel=="max_crit"]
    out <- x[1,c("IR_MLID","BeneficialUse","Year")]
    out$Scenario = "A"
    ncount = length(x$ActivityStartDate)
    out$SampleCount = ncount
    out$ExcCountLim = ifelse(out_48_hr>=5,rounding(ncount,max_exc_pct),1)
    out$ExcCount = length(x$IR_Value[x$IR_Value>stdcrit])
    if(out_48_hr<5){
      out$IR_Cat = ifelse(out$ExcCount>=out$ExcCountLim,"IDEX","IDNE")
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
      out$SampleCount = length(geomeans[geomeans>0])
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
        out$IR_Cat = ifelse(geomean>stdcrit,"IDEX","FS")
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

    # Bind scenarios together into one object and add to ecoli_assessments list
    ScenABC = dplyr::bind_rows(ScenA, ScenB, ScenC)

    ecoli_assessments$ecoli_allscenario_asmnts = ScenABC

### Rank and Roll Up By Scenarios (one scenario assessment by MLID/use/year as determined by E. coli scenario hierarchy)###
  
  ScenABC = ScenABC[!(ScenABC$IR_Cat=="ScenB"|ScenABC$IR_Cat=="ScenC"),]
  ScenABC$S.rank = 4
  ScenABC$S.rank[ScenABC$Scenario=="B"] = 3
  ScenABC$S.rank[ScenABC$Scenario=="C"] = 2
  ScenABC$S.rank[ScenABC$IR_Cat=="Not Assessed"] = 1
  
  ScenABC_agg <- aggregate(S.rank~IR_MLID+BeneficialUse+Year, data=ScenABC, FUN=max)
  ScenABC_agg$Scenario = "A"
  ScenABC_agg$Scenario[ScenABC_agg$S.rank==3]="B"
  ScenABC_agg$Scenario[ScenABC_agg$S.rank==2]="C"
  ScenABC_agg$Scenario[ScenABC_agg$S.rank==1]="C"
  ScenABC_agg = ScenABC_agg[,!names(ScenABC_agg)%in%"S.rank"]
  
  ScenABC_assess <- merge(ScenABC_agg, ScenABC, all.x=TRUE)
  ScenABC_assess = ScenABC_assess[,!names(ScenABC_assess)%in%c("S.rank")]
  
  ecoli_assessments$ecoli_scenario_rollup = ScenABC_assess

### Roll up to site level assessments ###
  
  # Merge data back to original dataset
  data_raw1 <- unique(data_raw[,c("IR_MLID","ASSESS_ID","BeneficialUse","BEN_CLASS","R3172ParameterName")])
  
  # Using all scenarios for MLID/Use roll-up
  ecoli.assessed <- merge(ecoli_assessments$ecoli_allscenario_asmnts,data_raw1, all.x=TRUE)
  ecoli.assessed <- ecoli.assessed[ecoli.assessed$IR_Cat=="NS"|
                                    ecoli.assessed$IR_Cat=="IDNE"|
                                    ecoli.assessed$IR_Cat=="IDEX"|
                                     ecoli.assessed$IR_Cat=="FS",]
  
  length(unique(ecoli.assessed$IR_MLID))#953
  
  # For MLIDS that have a mix of years that are FS and IDNE, pick the most recent year to represent that category
  #Find MLID-uses
  mlid_cats = unique(ecoli.assessed[,c("IR_MLID","BeneficialUse","IR_Cat")])
  mlid_cats$present = 1
  cat_table = tidyr::pivot_wider(mlid_cats, names_from = "IR_Cat", values_from = "present")
  
  fsid_mlids = subset(cat_table, cat_table$IDNE==1&cat_table$FS==1&is.na(cat_table$NS)&is.na(cat_table$IDEX))
  
  mlid_uses = fsid_mlids[,c("IR_MLID","BeneficialUse")] # 158
  
  ecoli.assessed_fsid = merge(mlid_uses, ecoli.assessed, all.x = TRUE)
  fsid.agg = ecoli.assessed_fsid%>%group_by(IR_MLID,BeneficialUse)%>%filter(Year==max(Year))
  
  # Pull out MLID/Uses assessed above
  mlids2rem = unique(fsid.agg[,c("IR_MLID","BeneficialUse")])
  mlids2rem$remove = 1
  nsid = merge(ecoli.assessed, mlids2rem, all.x = TRUE)
  nsid$remove = ifelse(is.na(nsid$remove),0,1)
  nsid_data = nsid[nsid$remove==0,]
  
  # Represent categories numerically so we can select the "max" category to define the AU
  # Hierarchy of decision making within each subset: NS>TMDLa>IDEX>FS>IDNE
  nsid_data$AssessCat[nsid_data$IR_Cat=="NS"]<-4
  nsid_data$AssessCat[nsid_data$IR_Cat=="IDEX"]<-3
  nsid_data$AssessCat[nsid_data$IR_Cat=="FS"]<-2
  nsid_data$AssessCat[nsid_data$IR_Cat=="IDNE"]<-1
  
  # Group not supports by MLID and Use and pick the max ranked assessment category
  # nsid.agg = nsid_data%>%group_by(IR_MLID,BeneficialUse)%>%filter(AssessCat==max(AssessCat))
  # nsid.agg = nsid_data%>%group_by(IR_MLID,BeneficialUse)%>%filter(AssessCat==max(AssessCat), Year==max(Year))
  # nsid.agg = nsid.agg[,!names(nsid.agg)%in%"AssessCat"]
  
  nsid.agg <- aggregate(AssessCat~IR_MLID+ASSESS_ID+BeneficialUse+BEN_CLASS+R3172ParameterName, data=nsid_data, FUN=max)
  names(nsid.agg)[names(nsid.agg)=="AssessCat"]<- "IR_Cat"
  
  #Renaming assessment categories
  nsid.agg$IR_Cat=as.character(nsid.agg$IR_Cat)
  nsid.agg=within(nsid.agg,{
    IR_Cat[IR_Cat=="4"]="NS"
    IR_Cat[IR_Cat=="3"]="IDEX"
    IR_Cat[IR_Cat=="2"]="FS"
    IR_Cat[IR_Cat=="1"]="IDNE"
  })
  
  all.agg = plyr::rbind.fill(fsid.agg, nsid.agg)
  
  # Add in some columns for app
  mlid_info = unique(data_rec[,c("IR_MLID","IR_MLNAME","IR_Lat","IR_Long","AU_NAME")])
  ecoli.assess.agg = merge(all.agg, mlid_info, all.x = TRUE)
  
  ecoli_assessments$ecoli_mlid_asmnts = ecoli.assess.agg
  
  return(ecoli_assessments)
} 

#### ALTERNATIVE METHOD FOR PULLING OUT IDNE/FS CANDIDATES
# mlid_cats = unique(ecoli.assessed[,c("IR_MLID","BeneficialUse","IR_Cat")])
# #mlid_cats$present = 1
# length(unique(mlid_cats$IR_MLID))
# 
# cat_table_wide=reshape2::dcast(mlid_cats, IR_MLID+BeneficialUse~IR_Cat, fun.aggregate=length, value.var='IR_Cat')
# head(cat_table_wide)
# summary(cat_table_wide)
# length(unique(cat_table_wide$IR_MLID))
# 
# cat_table_wide=within(cat_table_wide, {
#   to_reduce=ifelse(FS==1 & IDNE==1 & NS==0 & IDEX==0, 1, 0)
# })
# 
# dim(ecoli.assessed)
# ecoli.assessed=merge(ecoli.assessed, cat_table_wide, all.x=T)
# dim(ecoli.assessed)
# 
# to_reduce=subset(ecoli.assessed, to_reduce==1)
# ecoli.assessed=subset(ecoli.assessed, to_reduce==0)
# dim(ecoli.assessed)[1] + dim(to_reduce)[1]
# 
# # One way:
# selected_year=aggregate(Year~IR_MLID+BeneficialUse, to_reduce, FUN='max')
# dim(selected_year)
# dim(unique(selected_year[,c('IR_MLID', 'BeneficialUse')]))
# names(selected_year)[names(selected_year)=='Year']='sel_year'
# 
# reduced = merge(to_reduce, selected_year, all.x=T)
# dim(reduced)[1]==dim(to_reduce)[1]
# 
# reduced=subset(reduced, Year==sel_year)
# reduced=reduced[,names(ecoli.assessed)]
# 
# ecoli.assessed=rbind(ecoli.assessed, reduced)
# length(unique(ecoli.assessed$IR_MLID))
# 
# nsid_data = ecoli.assessed