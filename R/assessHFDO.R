#' Run HFDO assessments
#'
#' Performs high frequency dissolved oxygen assessments per IR assessment methods. This includes checking for data sufficiency, calculating daily minima and averages, and 7-day/30-day moving averages.
#'
#' @param data HFDO data--for the time being is a test dataset built by EH using "hfdo_prep.R".
#' @param consecday Numeric. Minimum number of consecutive days needed to perform an assessment.
#' @return List object containing data used for minimum, 7-day, and 30-day assessments, full list of assessments by type, and a rollup to site-use assessments.
#' @import data.table
#' @importFrom plyr ddply
#' @importFrom lubridate hour
#' @export assessHFDO

#### TESTING ####
# library(lubridate)
# library(plyr)
# library(data.table)
# load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
# data = hfdo_data
# consecday = 39

assessHFDO <- function(data, consecday=39){
  
HFDO_assessed <- list()

# Remove NA IR_Values
data = data[!is.na(data$IR_Value),]

#data$Year = year(data$ActivityStartDate) # to calculate consecutive dates by year  
data$AsmntAggPeriod = as.character(data$AsmntAggPeriod) # for subsetting by assessment

# Create new column with hour and day combined to calculate spacing between measurements.
data$Day = as.numeric(data$ActivityStartDate) # Convert date to unique identifier of day number since Excel "beginning of time" 1899-12-30
data$Hour = as.numeric(lubridate::hour(strptime(data$ActivityStartTime.Time, format="%H:%M")))/24 # Get hour sample collected (out of 24), and divide by 24 to obtain a proportion of day between 0 and 1.
data$yday_hr <- as.numeric(data$Day+data$Hour) # Add proportion to numeric year day -- now, mathematically, a sample collected in the 24th hour of one day can be easily comparable in time to a sample collected in the 1st hour of the next day.

## Testing ##
#data1 = data[data$BeneficialUse=="3B"&data$IR_MLID=="UTAHDWQ_WQX-4992320"&data$AsmntAggPeriod=="30",]
# Spacing check and aggregate function (aggregate by site, year, use)
sc_agg <- function(x){
  
  # Copy and cut down dataset based on sample spacing/density rules
  spacing.test = x[order(x$yday_hr),] # order data by numeric yday.hr value
  spacing.test$diff = c(0.00001,diff(spacing.test$yday_hr)) # determine difference between value and the one before it--starts with number just greater than zero to ensure first record kept.
  spacing.test = spacing.test[!spacing.test$diff==0,] # remove samples collected in "duplicated" hours (added back in later--do not count toward consecutive 1-sample per hour tally)
  spacing.test$diff_log <- spacing.test$diff<0.042 # create logical vector to "flag" when difference greater than the proportion of 1 hour (e.g. 1/24) between two values 
  
  # Create dataframe for spacing calculations
  rles <- rle(spacing.test$diff_log) # count consecutive numbers of TRUE and FALSE values
  rledat <- data.frame(rles$lengths,rles$values)
  rledat <- within(rledat,{
    index_upper = cumsum(rles$lengths) # determine where consecutive pattern stops
    index_lwr = index_upper-rles$lengths # determine where consecutive pattern starts (starts on FALSE value, which signifies that value is more than 1 hour away from the sample value before it)
    })
  rledat$index_lwr[rledat$index_lwr==0]=1 # zero will throw off function because there is no zero position in data frame
  
  # Pull out value ranges that fit spacing/density rules (determined by consecday input)
  suf_dat <- rledat[rledat$rles.values=="TRUE"&rledat$rles.lengths>=consecday*24,] # isolate to lengths of TRUE greater than the number of consecutive days x 24 hours
  
  if(dim(suf_dat)[1]>0){
    suf.yday.lwr <- spacing.test$yday_hr[c(suf_dat$index_lwr)] # pull out the yday.hr values corresponding to the top of the range
    suf.yday.upper <- spacing.test$yday_hr[c(suf_dat$index_upper)] # pull out the yday.hr values corresponding to the bottom layer of the range
    
    #Subset data to ranges that fit spacing/density rules using data.table %inrange%
    x = data.table(x)
    range = data.table(start = suf.yday.lwr,end=suf.yday.upper)
    data1 <- x[yday_hr%inrange%range]
    data1 <- data1[order(data1$yday_hr),]
    
    #### ALTERNATE WAY ##### (about 2% slower with 200k records)
    # dat_list = list()
    # for(i in 1:length(suf.yday.lwr)){
    #   x$logic <- x$yday_hr>=suf.yday.lwr[i]&x$yday_hr<=suf.yday.upper[i]
    #   dat_list[[i]] <- x[x$logic==TRUE,]
    # }
    # data1 = do.call(rbind, dat_list)
    
    return(data1)
  }else{return(NULL)}
}

#### MIN DO ASSESSMENT ####
# Determine which sets of data meet spacing requirements...no need to separate seasonal site specific standards for this assessment since all data points are assessed against individually assigned standards.
# (this is different for the aggregated standards below...)
dat.spaced <- plyr::ddply(.data=data,.(IR_MLID,BeneficialUse,AsmntAggPeriod),.fun=sc_agg)
# Aggregate to daily mins
dat.spaced.mins <- dat.spaced[dat.spaced$AsmntAggPeriod=="1",]
day.mins <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+NumericCriterion+CriterionUnits,data=dat.spaced.mins, FUN=min)
HFDO_assessed$min_do_data = day.mins

min.do <- function(x){
 out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse","NumericCriterion")]
 x$Exc = ifelse(x$IR_Value<x$NumericCriterion,1,0)
 tenpct = ceiling(dim(x)[1]*.1)
 out$Ncount = dim(x)[1]
 out$ExcCount = sum(x$Exc)
 out$IR_Cat = ifelse(sum(x$Exc)>tenpct,"NS","FS")
 return(out)
 }

min.do.assessed <- plyr::ddply(.data=day.mins, .(IR_MLID,BeneficialUse), .fun=min.do)
min.do.assessed$CriterionLabel = "MinDO"

#################### SPACING FOR MOVING WINDOW STDS #####################
# Again, determine which sets of data meet spacing requirements
# FOR SITES WITH SITE SPECIFIC STDS ONLY : since aggregated means compared to one standard (and site specific standards for JR change seasonally), need to perform spacing test based on season ranges.
nonss.data <- data[is.na(data$ss_R317Descrp),] # data without SS stds
nonss.spaced <- plyr::ddply(.data=nonss.data,.(IR_MLID,BeneficialUse,AsmntAggPeriod),.fun=sc_agg) # only aggregate by MLID, Use, and AsmntAggPeriod

ss.data <- data[!is.na(data$ss_R317Descrp),] # data with SS stds
ss.spaced <- plyr::ddply(.data=ss.data,.(IR_MLID,BeneficialUse,AsmntAggPeriod,CriterionLabel),.fun=sc_agg)

# Combine non ss and ss spacing data
dat.spaced.mov = plyr::rbind.fill(nonss.spaced,ss.spaced)

# Aggregate to daily averages
day.means <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+AsmntAggPeriod+NumericCriterion+CriterionUnits,data=dat.spaced.mov, FUN=mean)
HFDO_assessed$moving_avg_data = day.means[!day.means$AsmntAggPeriod=="1",]

#### 7-DAY MEANS ####
means7d <- day.means[day.means$AsmntAggPeriod=="7",]
# x = means7d[means7d$IR_MLID=="UTAHDWQ_WQX-4991900"&means7d$BeneficialUse=="3B",]

assess7d <- function(x){
  out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse","NumericCriterion")]
  datmean = c()
  m = 1
  for(i in 1:dim(x)[1]){
    dmin = x$ActivityStartDate[i]
    dmax = x$ActivityStartDate[i]+6
    datrange <- x[x$ActivityStartDate>=dmin&x$ActivityStartDate<=dmax,]
    if(dim(datrange)[1]<7){next}
    datmean[m] <- mean(datrange$IR_Value)
    m = m+1
  }
  tenpct = ceiling(length(datmean)*.1) 
  out$Ncount = length(datmean)
  out$ExcCount = length(datmean[datmean<out$NumericCriterion])
  out$IR_Cat = ifelse(out$ExcCount>tenpct,"NS","FS")
  return(out)
}

sevday.do.assessed <- plyr::ddply(.data=means7d, .(IR_MLID,BeneficialUse), .fun=assess7d)

if(dim(sevday.do.assessed)[1]>0){
  sevday.do.assessed$CriterionLabel = "7DA" 
}

###### 30-DAY MEANS #######
# Aggregate to daily averages
means30d <- day.means[day.means$AsmntAggPeriod=="30",]
# x = means7d[means7d$IR_MLID=="UTAHDWQ_WQX-4991900"&means7d$BeneficialUse=="3B",]

assess30d <- function(x){
  out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse","NumericCriterion")]
  datmean = c()
  m = 1
  for(i in 1:dim(x)[1]){
    dmin = x$ActivityStartDate[i]
    dmax = x$ActivityStartDate[i]+29
    datrange <- x[x$ActivityStartDate>=dmin&x$ActivityStartDate<=dmax,]
    if(dim(datrange)[1]<30){next}
    datmean[m] <- mean(datrange$IR_Value)
    m = m+1
  }
  tenpct = ceiling(length(datmean)*.1) 
  out$Ncount = length(datmean)
  out$ExcCount = length(datmean[datmean<out$NumericCriterion])
  out$IR_Cat = ifelse(out$ExcCount>tenpct,"NS","FS")
  return(out)
}

thirtyday.do.assessed <- plyr::ddply(.data=means30d, .(IR_MLID,BeneficialUse), .fun=assess30d)
if(dim(thirtyday.do.assessed)[1]>0){
  thirtyday.do.assessed$CriterionLabel = "30DA"
}


### COMBINE ASSESSMENTS ### (add BEN_CLASS back in)
allHFDO_asmnts = plyr::rbind.fill(min.do.assessed,sevday.do.assessed,thirtyday.do.assessed)
benclass <- unique(data[,c("IR_MLID","BEN_CLASS")])
allHFDO_asmnts = merge(allHFDO_asmnts,benclass, all.x=TRUE)

HFDO_assessed$allHFDO_asmnts = allHFDO_asmnts

### Roll up to site-use assessment ###
allHFDO_asmnts.list <- list(allHFDO_asmnts)
site_use_rollup = rollUp(allHFDO_asmnts.list,group_vars = c("IR_MLID","BeneficialUse","R3172ParameterName"),expand_uses=TRUE,print=TRUE)
HFDO_assessed$site_use_rollup = site_use_rollup

#save(file="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\assessments\\HFDO_assessed_example.Rdata", HFDO_assessed)

return(HFDO_assessed)
}
