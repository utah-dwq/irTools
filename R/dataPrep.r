#' Final data prep step before assessments
#'
#' Performs unit conversions & daily data aggregations. Also checks native vs. target activities and fractions, dissolved vs. total value checks, river/stream depth & flow checks, and generates value based data flags. 
#'
#' @param data A merged, translated, and numeric criteria assigned WQP results R-object. Target units for conversions are defined by units associated with assigned numeric critera.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).
#' @param unit_sheetname Name of sheet in workbook holding IR unit conversion table. Defaults to "unitConvTable".
#' @param startRow Row to start reading the unit conversion table excel sheet from (in case headers have been added). Defaults to 1.


#' @return WQP data input with .

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom plyr rbind.fill

#' @export
dataPrep=function(data, translation_wb, unit_sheetname="unitConvTable", startRow=1){

result=list()


#SETUP
#data_crit <- read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\data_criteria.csv")
#data=data_crit
##translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\03translation\\ir_translation_workbook.xlsx"
#
#unit_sheetname="unitConvTable"
#startRow=1



reasons=data.frame(data[0,])
reasons$reason=character(0)


####################################
######Activity type check###########
####################################
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[IR_ActivityType!=TargetActivityType]="Non-assessed activity type for parameter"
	})
data_n=data_n[!is.na(data_n$reason),]
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
print(table(reasons$reason))
rm(data_n)


####################################
######Fraction type check###########
####################################

###JV note - interesting check here - I ran into a similar issue w/ a screen table where the IR_FLAG column was fully filled out so applyScreenTable() didn't trip an error,
###but another column that further code depends on was not fully filled out. Note sure the best way to deal with this yet. Also have some params for which there shouldn't be a fraction at all anyway.
###Added conditions !is.na(TargetFraction) and is.na(IR_Fraction) to try to account for this, but may want a more general solution. Also tweaked the warning to note that these records are getting rejected here.
#if(table(data$Data_Prep_FLAG)[1]+table(data$Data_Prep_FLAG)[2]!=dim(data)[1]){
#  print("WARNING: NAs coerced in Data_Prep_FLAG due to NA's in IR_Fraction or Target Fraction")
#}


if(any(is.na(data$IR_Fraction) & !is.na(data$TargetFraction))){
  print("WARNING: Records rejected due to incomplete IR_Fraction in translation table.")
}

data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[(IR_Fraction!=TargetFraction | is.na(IR_Fraction)) & !is.na(TargetFraction)]="Non-assessed fraction or fraction not defined, & fraction specified by criterion"
	})
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
print(table(reasons$reason))
rm(data_n)


#Load translation workbook updated from comparison of TOTAL/DISSOLVED units above.
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
}


##################################
###Unit conversion table checks###
##################################

#Read unit conversion table from translation workbook (loaded above)
unit_convs=data.frame(openxlsx::readWorkbook(translation_wb, sheet=unit_sheetname, startRow=startRow, detectDates=TRUE))

# UNIT CONV TABLE CHECKS: make sure all IR_UnitConv_FLAG and UnitConversionFactor (for ACCEPT combinations) are populated
if(any(is.na(unit_convs$IR_FLAG))){
  stop("Unit conversion table missing required IR_FLAG information. Please correct the table before proceeding.")
}
if(any((is.na(unit_convs$UnitConversionFactor) & unit_convs$IR_FLAG=="ACCEPT")|(is.na(unit_convs$UnitConversionFactor) & is.na(unit_convs$IR_FLAG)))){
  stop("Unit conversion table missing conversion factor(s) for potentially accepted IR_Unit/CriterionUnits combination(s). Please correct the table before proceeding.")
}

#unit_convs=subset(unit_convs,unit_convs$InData=="Y") - I don't think this is needed, but shouldn't matter much either. I guess conceivably you could have just checked whether various combos were in the wb, but now you're applying to a differet dataset?
unit_convs=unit_convs[!names(unit_convs) %in% c("DateAdded","InData")]
unit_convs[unit_convs==""]=NA
names(unit_convs)[names(unit_convs)=="IR_FLAG"]="IR_UnitConv_FLAG"


######################################################
###Data prep for dissolved vs. total fraction check###
######################################################

# Reduce dataframe to columns of interest for comparing total and dissolved, narrow down rows to unique values (currently values are duplicated if measurement subject to multiple uses/standards)
unique(data$ResultSampleFractionText)# note that NA's may be in this list.
data1 <- data[,names(data)%in%c("ActivityStartDate","ActivityIdentifier", "ActivityStartTime.Time", "R3172ParameterName","IR_Fraction","IR_Unit", "IR_Value")]
data1 <- unique(data1)

# Separate into TOTAL and DISSOLVED objects, and give unit/value columns unique names specific to total or dissolved
#***NOTE***IR_Unit column in tot retains original "IR_Unit", while IR_Unit in diss converted to "CriterionUnits" (consistent with updateUnitConvTable function)
tot <- subset(data1, data1$IR_Fraction=="TOTAL")
dim(tot)
tot <- tot[,!names(tot)%in%c("IR_Fraction")]
names(tot)[names(tot)=="IR_Value"]<- "IR_Value_Tot"

diss <- subset(data1, data1$IR_Fraction=="DISSOLVED")
dim(diss)
diss <- diss[,!names(diss)%in%c("IR_Fraction")]
names(diss)[names(diss)=="IR_Value"]<- "IR_Value_Diss"
names(diss)[names(diss)=="IR_Unit"]<- "CriterionUnits"

# Merge TOTAL and DISSOLVED objects based on AID, Start Date, and Parameter name
diss_tot <- merge(tot,diss, by=c("ActivityIdentifier","ActivityStartDate","R3172ParameterName"))
dim(diss_tot)

# Double check that blanks are all NA in data (shouldn't really need this at this point)
diss_tot[diss_tot==""]=NA


# Merge conversion table to dissolved / total data
diss_tot_units=merge(diss_tot,unit_convs,all.x=T)
dim(diss_tot_units)
table(diss_tot_units$IR_UnitConv_FLAG)
diss_tot_units <- diss_tot_units[diss_tot_units$IR_UnitConv_FLAG=="ACCEPT",]
dim(diss_tot_units)

#Convert IR_Unit/Value_Tot to same units as IR_Unit/Value_Diss
diss_tot_units$IR_Value_Tot=diss_tot_units$IR_Value_Tot*diss_tot_units$UnitConversionFactor
diss_tot_units$IR_Unit=diss_tot_units$CriterionUnits


#Calc +/- 5% and compare
diss_tot_units=within(diss_tot_units,{
	diss_val_minus5pct=IR_Value_Diss-IR_Value_Diss*0.05
	tot_val_plus5pct=IR_Value_Tot+IR_Value_Tot*0.05
	reason=NA
	reason[diss_val_minus5pct>tot_val_plus5pct]="Dissolved fraction result (-5%) > total fraction result (+5%)"
	})


diss_tot_units=diss_tot_units[!is.na(diss_tot_units$reason),names(diss_tot_units)%in%c("ActivityStartDate","ActivityIdentifier", "R3172ParameterName","reason")]
dim(diss_tot_units)

diss_tot_units=merge(diss_tot_units,data,all.x=T)
dim(diss_tot_units)

reasons=rbind(reasons, diss_tot_units[!is.na(diss_tot_units$reason),])
print(table(reasons$reason))


##################################
###Unit conversions for IR data###   ###Need work around for Temperature, water, deg F if it occurs (I just rejected it in paramTransTable for now)
##################################

#Double check that blanks are all NA in data (shouldn't really need this at this point)
data[data==""]=NA

#Merge conversion table to data (up through this point, same functionality as applyScreenTable)
data=merge(data,unit_convs,all.x=T)
dim(data)

#Reject records where IR_UnitConv_FLAG is REJECT
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[IR_UnitConv_FLAG=="REJECT"]="Inappropriate units for conversion or assessment"
	})
data_n=data_n[!is.na(data_n$reason),names(data_n) %in% names(reasons)]
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
print(table(reasons$reason))
rm(data_n)

#JV - in hindsight, shouldn't really be needed. Converted to warning.
#When IR_Unit = CriterionUnit, make UnitConversionFactor 1
if(any(na.omit(data[data$IR_Unit==data$CriterionUnits,"UnitConversionFactor"]!=1))){
	warning("WARNING: Potential error in unit conversion table. Conversion factor !=1 for record where IR_Unit==CriterionUnits.")
	}

#Convert IR_Value using Unit Conversion Value
data$IR_Value <- data$IR_Value*data$UnitConversionFactor
data$IR_Unit = data$CriterionUnits

head(data[is.na(data$IR_Value) & data$IR_DetCond!="NRV" & data$IR_UnitConv_FLAG!="REJECT",])


####Apply rejections to flag column in data
flags=reasons
flags$IR_DataPrep_FLAG="REJECT"
flags=unique(flags[,c("ActivityStartDate","ActivityIdentifier", "ActivityStartTime.Time", "R3172ParameterName","IR_Fraction","IR_DataPrep_FLAG")])
dimcheck=dim(data)[1]
data=merge(data,flags,all.x=T)
result$data_flags=data
result$flag_reasons=reasons

if(dimcheck!=dim(data)[1]){
	stop("ERROR: Error in applying data prep flags. Data dimension[1] has changed.")
	}
data=within(data, {IR_DataPrep_FLAG[is.na(IR_DataPrep_FLAG)]="ACCEPT"})

print("Data prep record ACCEPT/REJECT counts:")
print(table(data$IR_DataPrep_FLAG))
table(data[data$IR_DataPrep_FLAG=="ACCEPT","IR_UnitConv_FLAG"])


###Pull out accepted data
acc_data=data[data$IR_DataPrep_FLAG=="ACCEPT",]

#Subset columns
acc_data=acc_data[,c("OrganizationIdentifier","ActivityIdentifier","ActivityStartDate","ActivityStartTime.Time","IR_ActivityType","IR_MLID","R317Descrp","IR_Lat","IR_Long",
													"ASSESS_ID","AU_NAME","AU_Type","BeneficialUse",
													"R3172ParameterName","IR_Value","IR_Unit","IR_DetCond","IR_Fraction","CriterionUnits","TargetFraction",
													"DataLoggerLine","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode",
													"AssessmentType","CriterionLabel","CriterionType","DailyAggFun","AsmntAggPeriod","AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon","SSC_EndMon","SSC_MLID"
													)]

######
###Extract lake profiles
result$lake_profiles=acc_data[!is.na(acc_data$DataLoggerLine),]

#Remove profiles from acc_data
acc_data=acc_data[!acc_data$ActivityIdentifier %in% result$lake_profiles$ActivityIdentifier,]
sum(table(acc_data$DataLoggerLine))


facToNum=function(x){return(as.numeric(levels(x))[x])}

#x=toxics_strms
#value_var="IR_Value"
#drop_vars=c("OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time")
#agg_var="DailyAggFun"
aggDVbyfun=function(x, value_var, drop_vars, agg_var){
	val=x[,value_var]
	x=x[,!names(x) %in% value_var & !names(x) %in% drop_vars]
	num_names=names(x[unlist(lapply(x, is.numeric))])
	x=as.data.frame(lapply(x, addNA, ifany=T)) #Add NA as factor level where cols contain NAs (converts everything to factor)
	x=data.frame(val,x) #Add back in preserved numeric val (alternatively could allow it to convert to factor then use as.numeric(levels(z))[z])
	x=x[,!names(x) %in% drop_vars]
	daily=x[0,]
	funs=unique(x[,agg_var])
	
	for(n in 1:length(funs)){
		fun_n=funs[n]
		x_n=x[x[,agg_var]==fun_n,]
		daily_n=aggregate(val~.,x_n, FUN=get(paste(fun_n)))
		daily=rbind(daily,daily_n)
	}
	
	daily[num_names]=lapply(daily[num_names], facToNum) #Convert numeric cols back to numeric
	
	names(daily)[names(daily)=="val"]=value_var #Rename value_var
	
	return(daily)
}


#############
#######Toxics & correction factors
######
toxics_raw=acc_data[which(acc_data$AssessmentType=="Toxic" | acc_data$BeneficialUse=="CF"),]
 
#split streams & lakes
toxics_strms=toxics_raw[which(toxics_raw$AU_Type=="River/Stream"),]
toxics_lakes=toxics_raw[which(toxics_raw$AU_Type=="Reservoir/Lake"),]

###Streams
#Aggregate to daily values
dim(toxics_strms)


toxics_strms_daily=aggDVbyfun(toxics_strms,	value_var="IR_Value",drop_vars=c("OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode"), agg_var="DailyAggFun")

summary(toxics_strms[toxics_strms$R3172ParameterName=="Arsenic" & toxics_strms$IR_Unit=="MG/L","IR_Value"])
summary(toxics_strms_daily[toxics_strms_daily$R3172ParameterName=="Arsenic"& toxics_strms_daily$IR_Unit=="MG/L","IR_Value"])

summary(toxics_strms[toxics_strms$R3172ParameterName=="pH","IR_Value"])
summary(toxics_strms_daily[toxics_strms_daily$R3172ParameterName=="pH","IR_Value"])

dim(toxics_strms_daily)


#Assign CFs
cfs_strms=toxics_strms_daily[toxics_strms_daily$BeneficialUse=="CF",]
toxics_strms_daily=toxics_strms_daily[toxics_strms_daily$BeneficialUse!="CF",]
dim(toxics_strms_daily)

cfs_strms$cf=paste0("cf_",cfs_strms$DailyAggFun,"_",cfs_strms$R3172ParameterName,"_",cfs_strms$IR_Unit)
cfs_strms_cast=reshape2::dcast(cfs_strms, ActivityStartDate+IR_MLID~cf, value.var="IR_Value")

dim(toxics_strms_daily)
toxics_strms_daily=merge(toxics_strms_daily,cfs_strms_cast,all.x=T)
dim(toxics_strms_daily)
toxics_strms_daily=toxics_strms_daily[toxics_strms_daily$BeneficialUse!="CF",] #Remove CF rows



###Lakes

#Aggregate to daily values (including depth)
toxics_lakes_daily=aggDVbyfun(toxics_lakes,	value_var="IR_Value",drop_vars=c("OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time"), agg_var="DailyAggFun")

#Assign CFs
cfs_lakes=toxics_lakes_daily[toxics_lakes_daily$BeneficialUse=="CF",]
cfs_lakes$cf=paste0("cf_",cfs_lakes$DailyAggFun,"_",cfs_lakes$R3172ParameterName,"_",cfs_lakes$IR_Unit)
cfs_lakes_cast=reshape2::dcast(cfs_lakes, ActivityStartDate+IR_MLID+ActivityRelativeDepthName+ActivityDepthHeightMeasure.MeasureValue+ActivityDepthHeightMeasure.MeasureUnitCode~cf, value.var="IR_Value")
dim(toxics_lakes_daily)
toxics_lakes_daily=merge(toxics_lakes_daily,cfs_lakes_cast,all.x=T)
dim(toxics_lakes_daily)
toxics_lakes_daily=toxics_lakes_daily[toxics_lakes_daily$BeneficialUse!="CF",] #Remove CF rows


##???Select preferred depths for toxics (shallow for ammonia, deep for all others)
#samps=unique(toxics_lakes_daily[,!names(toxics_lakes_daily) %in% c("IR_Value","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode")])
#samps$Count=NA
#
#for(n in 1:dim(samps)[1]){
#	samps_n=samps[n,]
#	res_n=merge(samps_n, toxics_lakes_daily)
#	samps$Count[n]=dim(res_n)[1]
#	}



#Aggregate by AsmntAggPeriod AsmntAggPeriodUnit AsmntAggFun (need to do)
#Calculate CF dependent criterion values (need to do)

#rbind lakes & streams back together to result (fills depth cols w/ NA for streams)
result$toxics=plyr::rbind.fill(toxics_strms_daily, toxics_lakes_daily)


	

#############
#######Conventionals
###### 
conv_raw=acc_data[which(acc_data$AssessmentType=="Conventional" & acc_data$BeneficialUse!="CF"),]
 
#split streams & lakes
conv_strms=conv_raw[which(conv_raw$AU_Type=="River/Stream"),]
conv_lakes=conv_raw[which(conv_raw$AU_Type=="Reservoir/Lake"),]


#Streams
#Aggregate to daily values (including depth)
conv_strms_daily=aggDVbyfun(conv_strms,	value_var="IR_Value",drop_vars=c("OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode"), agg_var="DailyAggFun")

#Lakes
#Aggregate to daily values (including depth)
conv_lakes_daily=aggDVbyfun(conv_lakes,	value_var="IR_Value",drop_vars=c("OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time"), agg_var="DailyAggFun")


#Aggregate by AsmntAggPeriod AsmntAggPeriodUnit AsmntAggFun (need to do)


#rbind lakes & streams back together to result (fills depth cols w/ NA for streams)
result$conventionals=plyr::rbind.fill(conv_strms_daily, conv_lakes_daily)

objects(result)




#Other possible return objects:
#1. E coli
#2. HF do



#Other possible checks - execution TBD
#Rivers/streams depth check
#Value based flags & rejections (if performing, desired flags/rejections should be input to param translation table)
#Estimated & calculated value check
#Holding times


return(result)

}


