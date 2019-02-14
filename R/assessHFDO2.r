#' Run HFDO assessments (JV try)
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

### TESTING ####
 library(lubridate)
 library(plyr)
 library(data.table)
 load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\hfdo_demo\\hfdo_data.Rdata")
 hfdo_data$DailyAggFun[hfdo_data$AsmntAggPeriod>1]="mean" #These should be means in the standards table
 head(hfdo_data)
 data = hfdo_data
 consecday = 39

#assessHFDO2 <- function(data, consecday=39){


data$time=lubridate::hm(data$ActivityStartTime.Time)
data$hour=lubridate::hour(data$time)
head(data)

test1=data[data$IR_MLID=="UTAHDWQ_WQX-4991900" & data$BeneficialUse=="3B" & data$AsmntAggPeriod==7 & data$NumericCriterion==5.5,]

# Count samples per hr+date
date_hour_count=aggregate(IR_Value~hour+ActivityStartDate+IR_MLID, test1, FUN="length", drop=F)
names(date_hour_count)[names(date_hour_count)=="IR_Value"]="SampleCount"
table(date_hour_count$SampleCount, exclude=NULL)
head(date_hour_count[is.na(date_hour_count$SampleCount),])
head(date_hour_count[!is.na(date_hour_count$SampleCount),])

date_hour_count=within(date_hour_count,{
	complete_hour=SampleCount
	complete_hour[is.na(complete_hour)]=0
	complete_hour[complete_hour>=1]=1
	})
head(date_hour_count[is.na(date_hour_count$SampleCount),])
head(date_hour_count[!is.na(date_hour_count$SampleCount),])

# Count hours w/ sample per date
hour_count=aggregate(complete_hour~ActivityStartDate+IR_MLID, date_hour_count, FUN='sum')
names(hour_count)[names(hour_count)=="complete_hour"]="complete_hours"
head(hour_count)

# ID complete days (those w/ 24 complete_hours)
complete_days=hour_count[hour_count$complete_hours==24,]
complete_days=complete_days[order(complete_days$ActivityStartDate),]

# Total # of complete days
dim(complete_days)[1]

# ID groups of complete days
groups=rle(as.vector(diff(append(1,complete_days$ActivityStartDate),1)))
groups=data.frame(groups$lengths)
groups=within(groups,{
	end_index=cumsum(groups.lengths)
	start_index=end_index-groups.lengths+1
	end_date=complete_days$ActivityStartDate[end_index]
	start_date=complete_days$ActivityStartDate[start_index]
})

# Max consecutive days
max(groups$groups.lengths)

groups$group=seq(1, dim(groups)[1], 1)
groups$group[groups$groups.length<16]=NA
head(groups)

#Subset data to complete days
test1=test1[test1$ActivityStartDate %in% complete_days$ActivityStartDate,]

#Generate daily means
daily_means=aggregate(IR_Value~IR_MLID+ActivityStartDate, test1, FUN='mean')

# Tag daily means with groups for assessment
## Flatten grouping keys
asmnt_group=vector()
for(n in 1:dim(groups)[1]){
	group_n=groups[n,]
	key_n=rep(group_n$group, group_n$groups.lengths)
	asmnt_group=append(asmnt_group, key_n)
}

length(asmnt_group)==dim(daily_means)[1]

## Assign group
daily_means$asmnt_group=asmnt_group

## Drop NA groups
daily_means=daily_means[!is.na(daily_means$asmnt_group),]
daily_means[daily_means$asmnt_group==4,]

# Assess each group (depends on criterion) - ddply splitting on site, use, and asmnt_group
# Each function should return min_date, max_date, ExcCount, SampleCount, & possibly assessment category (but that could also be done later based on exceedance and total counts).




# Target output:
# Assessment for each unique site, use, & criterion combination

# Outline:
# For each unique combo of site, use, & criterion:
# 1. Identify complete days
#	a. Aggregate data to date + hour w/ FUN="length"
#	b. Set any length >=1 to 1
#	c. Aggregate step b output to date w/ FUN="sum" (any day w/ 24 is a complete day)
# 2. Any site/use/criterion w/ <10, <16, or <39 complete days for min, 7d, and 30d criteria respectively cannot be assessed, these can be skipped to save computing time.
# 3. For each complete day, calculate daily min or mean (depending on criterion)
# 4. For daily min criteria - calculate # of exceedances, # of samples, and # of allowable exceedances (10%), determine assessment category
# 5. For 7d or 30d mean criteria:
#	a. Find groups of consecutive days (rle or diff)
#	b. Count # of appropriate sample sizes for criterion and # of allowable exceedances (10%)
#	c. Tag daily means with a grouping key of groups of appropriate sample sizes
#	d. For groups of appropriate sample size, calc mean of daily means
#	e. calculate # of exceedances and compare to # of allowable exceedances
#	f. determine assessment category

#Steps 1-5 are a function
#To complete assessment, ddply to hfdo_data grouping on everything except date, time, and IR value


}
