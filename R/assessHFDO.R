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

load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
data = hfdo_data
# data = hfdo_data[hfdo_data$BeneficialUse=="3D"&hfdo_data$IR_MLID=="UTAHDWQ_WQX-4991900",]
# data$Year = year(data$ActivityStartDate) # aggregate by year
# data = data[data$Year=="2016",]
consecday = 7

assessHFDO <- function(data, consecday){

# Create unique standards table for use in functions (removes redundancy of standards in data)
data$AsmntAggPeriod = as.character(data$AsmntAggPeriod)
uniq.crit <- unique(data[,c("BeneficialUse","AsmntAggPeriod","NumericCriterion")])
data = unique(data[,!names(data)%in%c("AsmntAggPeriod","AsmntAggPeriodUnit","NumericCriterion","CriterionUnits")])

# Remove NA IR_Values
data = data[!is.na(data$IR_Value),]

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
  
  # Pull out value ranges that fit spacing/density rules (determined by consecday input)
  suf_dat <- rledat[rledat$rles.values=="TRUE"&rledat$rles.lengths>=consecday*24,] # isolate to lengths of TRUE greater than the number of consecutive days x 24 hours
  suf_dat$suf.yday.lwr <- spacing.test$yday_hr[c(suf_dat$index_lwr)] # pull out the yday.hr values corresponding to the top of the range
  suf_dat$suf.yday.upper <- spacing.test$yday_hr[c(suf_dat$index_upper)] # pull out the yday.hr values corresponding to the bottom layer of the range
  
  # Subset data to ranges that fit spacing/density rules using data.table %inrange%
  data1 <- x[x$yday_hr%inrange%suf_dat[,c("suf.yday.lwr","suf.yday.upper")],]
  data1 <- data1[order(data1$yday_hr),]
  return(data1)
}

dat.spaced <- ddply(.data=data,.(IR_MLID,BeneficialUse,Year),.fun=sc_agg)

# Aggregate to daily mins and averages
day.mins <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+Year,data=data1, FUN=min)
day.means <- aggregate(IR_Value~R3172ParameterName+BeneficialUse+ActivityStartDate+IR_MLID+Year,data=data1, FUN=mean)


}