#' Run HFDO assessments
#'
#' Performs high frequency dissolved oxygen assessments per IR assessment methods. This includes checking for data sufficiency, calculating daily minima and averages, and 7-day/30-day moving averages.
#'
#' @param data HFDO data--for the time being is a test dataset built by EH.
#' @param consecday Numeric. Minimum number of consecutive days needed to perform an assessment.
#' @return 
#' @import data.table
#' @importFrom plyr ddply

#' @export
#' 
library(lubridate)
library(plyr)
library(data.table)
load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
data = hfdo_data
# data = hfdo_data[hfdo_data$BeneficialUse=="3B"&hfdo_data$IR_MLID=="UTAHDWQ_WQX-4992320",]
# data = data[data$Year=="2016",]
consecday = 7

############## TO DO ##################
# add in ss handling for spacing determination (if statement whether ss_descrp (or the startmon/endmon) populated)

assessHFDO <- function(data, consecday){

# Remove NA IR_Values
data = data[!is.na(data$IR_Value),]

data$Year = year(data$ActivityStartDate) # to calculate consecutive dates by year  
data$AsmntAggPeriod = as.character(data$AsmntAggPeriod) # for subsetting by assessment

# Create new column with hour and day combined to calculate spacing between measurements.
data$Day = as.numeric(yday(data$ActivityStartDate)) # Convert year day to number
data$Hour = as.numeric(hour(strptime(data$ActivityStartTime.Time, format="%H:%M")))/24 # Get hour sample collected (out of 24), and divide by 24 to obtain a proportion of day between 0 and 1.
data$yday_hr <- data$Day+data$Hour # Add proportion to numeric year day -- now, mathematically, a sample collected in the 24th hour of one day can be easily comparable in time to a sample collected in the 1st hour of the next day.

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

dat.spaced <- ddply(.data=data,.(IR_MLID,BeneficialUse,AsmntAggPeriod),.fun=sc_agg)

#### MIN DO ASSESSMENT ####
# Aggregate to daily mins
dat.spaced.mins <- dat.spaced[dat.spaced$AsmntAggPeriod=="1",]
day.mins <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+NumericCriterion+CriterionUnits,data=dat.spaced.mins, FUN=min)

min.do <- function(x){
 out <- x[1,c("IR_MLID","R3172ParameterName","BeneficialUse")]
 x$Exc = ifelse(x$IR_Value<x$NumericCriterion,1,0)
 tenpct = ceiling(dim(x)[1]*.1)
 out$Ncount = dim(x)[1]
 out$ExcCount = sum(x$Exc)
 out$Cat = ifelse(sum(x$Exc)>tenpct,"NS","FS")
 return(out)
 }

min.do.assessed <- ddply(.data=day.mins, .(IR_MLID,BeneficialUse), .fun=min.do)
min.do.assessed$CriterionLabel = "Min DO"

#### 7-DAY MEANS ####
# Aggregate to daily averages
day.means <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+AsmntAggPeriod+NumericCriterion+CriterionUnits,data=dat.spaced, FUN=mean)
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
  out$Cat = ifelse(out$ExcCount>tenpct,"NS","FS")
  return(out)
}

sevday.do.assessed <- ddply(.data=means7d, .(IR_MLID,BeneficialUse), .fun=assess7d)

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
  out$Cat = ifelse(out$ExcCount>tenpct,"NS","FS")
  return(out)
}

thirtyday.do.assessed <- ddply(.data=means30d, .(IR_MLID,BeneficialUse), .fun=assess30d)

}
