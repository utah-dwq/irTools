#' Final data prep step before assessments
#'
#' Performs unit conversions & daily data aggregations. Also checks native vs. target activities and fractions, dissolved vs. total value checks, river/stream depth & flow checks, and generates value based data flags.
#'
#' @param data A merged, translated, and numeric criteria assigned WQP results R-object. Target units for conversions are defined by units associated with assigned numeric critera.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).
#' @param unit_sheetname Name of sheet in workbook holding IR unit conversion table. Defaults to "unitConvTable".
#' @param startRow_unit Row to start reading the unit conversion table excel sheet from (in case headers have been added). Defaults to 1.
#' @param crit_wb Full path and filename for workbook containing criteria.
#' @param cf_formulas_sheetname Name of sheet in criterion workbook holding conversion factors and criterion formulas for criteria dependent on CFs.
#' @param startRow_formulas Row to start reading the formulas table from (in case headers have been added). Defaults to 1.
#' @param split_agg_tds Logical. If TRUE (default) split off TDS records w/ function assigned in AsmntAggFun into separate output. If FALSE, these records are passed through to conventionals output.

#' @return A list of objects ready for assessments.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom plyr rbind.fill

#' @export
dataPrep=function(data, translation_wb, unit_sheetname="unitConvTable", crit_wb, cf_formulas_sheetname, startRow_unit=1, startRow_formulas=1, split_agg_tds=TRUE){

##SETUP#####
#rm(list=ls(all=TRUE))
#load("P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\ready_for_prep.RData")
#data=data_crit
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\lookup_tables\\ir_translation_workbook.xlsx"
#split_agg_tds=TRUE
#unit_sheetname="unitConvTable"
#startRow_unit=1
#crit_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\lookup_tables\\IR_uses_standards.xlsx"
#cf_formulas_sheetname="cf_formulas"
#startRow_formulas=1
#######

result=list()

reasons=data.frame(data[0,])
reasons$reason=character(0)

#Remove records w/o criteria (in case they have been optionally passed through assignCriteria - these cause errors in aggregation steps as they do not have aggregation functions specified)
data=data[!is.na(data$NumericCriterion) | data$BeneficialUse=="CF" | data$R3172ParameterName=="Profile depth",]

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
unit_convs=data.frame(openxlsx::readWorkbook(translation_wb, sheet=unit_sheetname, startRow=startRow_unit, detectDates=TRUE))

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
if(dim(diss_tot)[1]>0){
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
}

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

if(any(data$IR_Unit!=data$IR_LowerLimitUnit, na.rm=T) | any(data$IR_Unit!=data$IR_UpperLimitUnit, na.rm=T)){
	stop("Error: Result and detection limit units do not match. Cannot convert or compare units to criterion units.")
}

#Convert IR_Value and upper and lower limit values using Unit Conversion Value
data=within(data,{
	IR_Value = IR_Value*UnitConversionFactor
	IR_Unit = CriterionUnits
	IR_LowerLimitValue = IR_LowerLimitValue*UnitConversionFactor
	IR_LowerLimitUnit = CriterionUnits
	IR_UpperLimitValue = IR_UpperLimitValue*UnitConversionFactor
	IR_UpperLimitUnit = CriterionUnits
})


#Reject non-detect records where IR_LowerLimitValue > NumericCriterion
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[IR_DetCond=="ND" & IR_LowerLimitValue>NumericCriterion]="Non-detect result with detection limit > criterion."
	})
data_n=data_n[!is.na(data_n$reason),names(data_n) %in% names(reasons)]
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
print(table(reasons$reason))
rm(data_n)


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

#Subset columns (note - col names may change w/ standards table, may want a cleaner way around this)
acc_data=acc_data[,c("OrganizationIdentifier","ActivityIdentifier","ActivityStartDate","ActivityStartTime.Time","IR_ActivityType","IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long",
													"ASSESS_ID","AU_NAME","AU_Type","BeneficialUse","BEN_CLASS","CharacteristicName",
													"R3172ParameterName","IR_Value","IR_Unit","IR_DetCond","IR_Fraction","CriterionUnits","TargetFraction",
													"DataLoggerLine","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode",
													"AssessmentType","CriterionLabel","CriterionType","DailyAggFun","AsmntAggPeriod","AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon","SSC_EndMon","SSC_MLID"
													)]

######
###Extract lake profiles
result$lake_profiles=acc_data[!is.na(acc_data$DataLoggerLine) & acc_data$BeneficialUse %in% c("3A","3B","3C","3D","3E"),]

table(result$lake_profiles$R3172ParameterName)

#Remove profiles from acc_data
acc_data=acc_data[!acc_data$ActivityIdentifier %in% result$lake_profiles$ActivityIdentifier,]
sum(table(acc_data$DataLoggerLine))


#Return accepted data (minus lake profiles)
result$accepted_data=acc_data

#Extract lakes trophic data
result$lakes_trophic=acc_data[acc_data$AU_Type=="Reservoir/Lake" & acc_data$R3172ParameterName %in% c("Chlorophyll a", "Total Phosphorus as P","Depth, Secchi disk depth"),]

#Extract e coli
result$ecoli=acc_data[acc_data$R3172ParameterName=="E. Coli",] #Note, need to name parameter in param translation table and update here.

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

drop_vars=c("DataLoggerLine","OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode","IR_Fraction","IR_DetCond")

#############
#######Toxics & correction factors
######
if(any(acc_data$AssessmentType=="Toxic")){

toxics_raw=acc_data[which(acc_data$AssessmentType=="Toxic" | acc_data$BeneficialUse=="CF"),]


	
	#split streams & lakes
	toxics_strms=toxics_raw[which(toxics_raw$AU_Type=="River/Stream"),]
	toxics_lakes=toxics_raw[which(toxics_raw$AU_Type=="Reservoir/Lake"),]
	
	if(dim(toxics_strms)[1]>0){
		###Streams
		#Aggregate to daily values
		dim(toxics_strms)
		toxics_strms_daily=aggDVbyfun(toxics_strms,	drop_vars=drop_vars,	value_var="IR_Value", agg_var="DailyAggFun")
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
	}
	
	
	###Lakes (JV note - applying same logic as streams, aggregate all parameters including CFs to daily values by DailyAggFun, then cast & merge CFs to data)
	
	if(dim(toxics_lakes)[1]>0){
		#Aggregate to daily values
		dim(toxics_lakes)
		toxics_lakes_daily=aggDVbyfun(toxics_lakes,	drop_vars=drop_vars, value_var="IR_Value", agg_var="DailyAggFun")
		dim(toxics_lakes_daily)
		
		#Assign CFs
		cfs_lakes=toxics_lakes_daily[toxics_lakes_daily$BeneficialUse=="CF",]
		toxics_lakes_daily=toxics_lakes_daily[toxics_lakes_daily$BeneficialUse!="CF",]
		dim(toxics_lakes_daily)
		
		cfs_lakes$cf=paste0("cf_",cfs_lakes$DailyAggFun,"_",cfs_lakes$R3172ParameterName,"_",cfs_lakes$IR_Unit)
		cfs_lakes_cast=reshape2::dcast(cfs_lakes, ActivityStartDate+IR_MLID~cf, value.var="IR_Value")
		
		dim(toxics_lakes_daily)
		toxics_lakes_daily=merge(toxics_lakes_daily,cfs_lakes_cast,all.x=T)
		dim(toxics_lakes_daily)
		toxics_lakes_daily=toxics_lakes_daily[toxics_lakes_daily$BeneficialUse!="CF",] #Remove CF rows
	}
	
	#Merge lakes & streams toxics
	if(exists("toxics_strms_daily") & exists("toxics_lakes_daily")){
		toxics=plyr::rbind.fill(toxics_strms_daily, toxics_lakes_daily)
	}
	if(exists("toxics_strms_daily") & !exists("toxics_lakes_daily")){
		toxics=toxics_strms_daily
	}
	if(!exists("toxics_strms_daily") & exists("toxics_lakes_daily")){
		toxics=toxics_lakes_daily
	}
	
	
	#Calculate hardness (will need to update for different hardness parameters, max of 400?):
	toxics=within(toxics,{
			hardness=100*(`cf_min_Calcium_mg/l`/40.08 + `cf_min_Magnesium_mg/l`/24.3)
			#hardness[is.na(hardness)]=OTHER HARDNESS COLUMNS HERE...
		})
	
	
	
	#Calculate CF dependent criterion values (need to do)
	test=toxics[toxics$R3172ParameterName=="Cadmium" | toxics$R3172ParameterName=="Arsenic" | toxics$R3172ParameterName=="Aluminum",]
	head(test)
	
	
	#Load criterion workbook
	criterion_wb=openxlsx::loadWorkbook(crit_wb)
	
	#Remove filters from all sheets in crit_wb
	sheetnames=openxlsx::getSheetNames(crit_wb)
	for(n in 1:length(sheetnames)){
	openxlsx::removeFilter(criterion_wb, sheetnames[n])
	}
	
	#Read formula table
	cf_formulas=data.frame(openxlsx::readWorkbook(criterion_wb, sheet=cf_formulas_sheetname, startRow=startRow_formulas, detectDates=TRUE))
	
	toxics=merge(toxics, cf_formulas, all.x=T)
	
	calc=toxics$NumericCriterion
	toxics=within(toxics, {
		CF[!is.na(CF)]=paste0("(",CF[!is.na(CF)],")")
		CriterionFormula=gsub("CF * ","",CriterionFormula, fixed=TRUE)
		CriterionFormula[!is.na(CF)]=paste0("(",CriterionFormula[!is.na(CF)],")")
		CriterionFormula[!is.na(CF)]=paste(CF[!is.na(CF)], "*", CriterionFormula[!is.na(CF)])
		CriterionFormula=gsub("ln", "log", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub(") (", ")*(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub(")(",  ")*(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("(e(",  "(exp(", CriterionFormula, fixed=TRUE)
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "hardness", as.character(hardness))
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "min_pH", as.character(cf_min_pH_NA))
		CalculatedCrit=sapply(CriterionFormula, function(x) eval(parse(text=x)))
		suppressWarnings({
			NumericCriterion=facToNum(NumericCriterion)
		})
		NumericCriterion[calc=="calc"]=CalculatedCrit[calc=="calc"]
	})
	
	#Generate toxics result
	result$toxics=toxics
}


#############
#######Conventionals
######

if(any(acc_data$AssessmentType=="Conventional")){
	conv_raw=acc_data[which(acc_data$AssessmentType=="Conventional" & acc_data$BeneficialUse!="CF"),]
	
	#split streams & lakes
	conv_strms=conv_raw[which(conv_raw$AU_Type=="River/Stream"),]
	conv_lakes=conv_raw[which(conv_raw$AU_Type=="Reservoir/Lake"),]
	
	if(dim(conv_strms)[1]>0){
		#Streams
		#Aggregate to daily values (including depth)
		conv_strms_daily=aggDVbyfun(conv_strms,	drop_vars=drop_vars,	value_var="IR_Value", agg_var="DailyAggFun")
	}
	
	if(dim(conv_lakes)[1]>0){
		#Lakes
		#Select surface only results for lakes conventionals
		suppressWarnings({
			conv_lakes$ActivityDepthHeightMeasure.MeasureValue=facToNum(conv_lakes$ActivityDepthHeightMeasure.MeasureValue)
			})
		conv_lakes=within(conv_lakes,{
			ActivityDepthHeightMeasure.MeasureValue[which(ActivityDepthHeightMeasure.MeasureUnitCode=="ft" | ActivityDepthHeightMeasure.MeasureUnitCode=="feet")]=ActivityDepthHeightMeasure.MeasureValue*0.3048
			ActivityDepthHeightMeasure.MeasureUnitCode[which(ActivityDepthHeightMeasure.MeasureUnitCode=="ft" | ActivityDepthHeightMeasure.MeasureUnitCode=="feet")]="m"
			ActivityDepthHeightMeasure.MeasureUnitCode[which(ActivityDepthHeightMeasure.MeasureUnitCode=="meters")]="m"
			})
		if(any(conv_lakes$ActivityDepthHeightMeasure.MeasureUnitCode!="m", na.rm=T)){stop("Error: lake depth units cannot be converted to meters. Additional conversion may be needed.")}
		conv_lakes=conv_lakes[which(conv_lakes$ActivityRelativeDepthName=="Surface" | (conv_lakes$ActivityDepthHeightMeasure.MeasureValue <=2 & conv_lakes$ActivityDepthHeightMeasure.MeasureValue>=0)),]
		
		if(dim(conv_lakes)[1]>0){
			#Remove data for ALUs (assessed via profile tools)
			conv_lakes=conv_lakes[!conv_lakes$BeneficialUse %in% c("3A","3B","3C","3D","3E"),]
		}
		
		if(dim(conv_lakes)[1]>0){
			#Aggregate to daily values (excluding depths this time - all surface samples)
			conv_lakes_daily=aggDVbyfun(conv_lakes,	drop_vars=drop_vars,	value_var="IR_Value", agg_var="DailyAggFun")
		}
		
		if(dim(conv_lakes)[1]>0){		
			#Separate lakes TDS (different assessment methods)
			result$lakes_tds=conv_lakes[conv_lakes$R3172ParameterName=="Total Dissolved Solids",]
			conv_lakes=conv_lakes[conv_lakes$R3172ParameterName!="Total Dissolved Solids",]
		}
	}
	
	#rbind lakes & streams back together to result (fills depth cols w/ NA for streams)
	
	if(exists("conv_strms_daily") & exists("conv_lakes_daily")){
		conventionals=plyr::rbind.fill(conv_strms_daily, conv_lakes_daily)
	}
	if(exists("conv_strms_daily") & !exists("conv_lakes_daily")){
		conventionals=conv_strms_daily
	}
	if(!exists("conv_strms_daily") & exists("conv_lakes_daily")){
		conventionals=conv_lakes_daily
	}
	
	if(split_agg_tds & exists("conventionals")){
		agg_tds=conventionals[conventionals$R3172ParameterName=="Total Dissolved Solids" & !is.na(as.character(conventionals$AsmntAggFun)),]
		conventionals=conventionals[conventionals$R3172ParameterName!="Total Dissolved Solids" | (conventionals$R3172ParameterName=="Total Dissolved Solids" & is.na(as.character(conventionals$AsmntAggFun))),]
		result$agg_tds=agg_tds
	}
	
	if(exists("conventionals")){result$conventionals=conventionals}
}

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
