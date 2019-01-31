#' Run HFDO assessments
#'
#' Performs high frequency dissolved oxygen assessments per IR assessment methods. This includes checking for data sufficiency, calculating daily minima and averages, and 7-day/30-day moving averages.
#'
#' @param data HFDO data--for the time being is a test dataset built by EH.
#' @return Returns a list of lake profile assessment dataframes. profile_asmnts_mlid_param contains site/parameter level profile assessments, profile_asmnts_individual contains assessments for each individual profile,
#' 	profile_criteria contains the criteria used for the profile assessment (excluding any site-specific criteria that may have occurred in the input dataset), 
#'  profiles_long contains profile data in long format including the numeric criterion associated with each parameter, profiles_wide contains profile data cast to wide format.
#' @importFrom reshape2 dcast
#' @importFrom dplyr rename
#' @importFrom rLakeAnalyzer thermo.depth
#' @importFrom plyr ddply

#' @export
#' 

load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
data = hfdo_data[hfdo_data$BeneficialUse=="3D"&hfdo_data$IR_MLID=="UTAHDWQ_WQX-4991900",]
data$Year = year(data$ActivityStartDate) # aggregate by year
data = data[data$Year=="2016",]
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

# Copy and cut down dataset based on sample spacing/density rules
spacing.test = data[order(data$yday_hr),] # order data by numeric yday.hr value
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

### TESTING ####
data1 <- data[data$yday_hr%inrange%suf_dat[,c("suf.yday.lwr","suf.yday.upper")],]
data1 <- data1[order(data1$yday_hr),]


#ranges = paste0("c(",paste(paste0(suf_dat$index_upper,":",suf_dat$index_lwr), sep="",collapse=","),")")

#dat_assess <- diff_dat[eval(parse(text=ranges),]

}