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
#' @importFrom wqTools facToNum

#' @export
dataPrep=function(data, translation_wb, unit_sheetname="unitConvTable", crit_wb, cf_formulas_sheetname, startRow_unit=1, startRow_formulas=1, split_agg_tds=TRUE){

######SETUP#####
#data=acc_data_criteria
#split_agg_tds=TRUE
#translation_wb='ir_translation_workbook_working_v12_ef - no IR_Fraction formula.xlsx'
#unit_sheetname="unitConvTable"
#startRow_unit=1
#crit_wb="IR_uses_standards_working_v4_ef.xlsx"
#cf_formulas_sheetname="cf_formulas"
#startRow_formulas=3
#########

result=list()


# aggregate to daily values function
#x=toxics_strms
#value_var="IR_Value"
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

	daily[num_names]=lapply(daily[num_names], wqTools::facToNum) #Convert numeric cols back to numeric

	names(daily)[names(daily)=="val"]=value_var #Rename value_var

	return(daily)
}

#Column names to pass through function
col_names=c("ResultIdentifier","OrganizationIdentifier","ActivityIdentifier","ActivityStartDate","ActivityStartTime.Time","IR_ActivityType","IR_MLID","IR_MLNAME","MonitoringLocationIdentifier","MonitoringLocationTypeName","R317Descrp","IR_Lat","IR_Long",
													"ASSESS_ID","AU_NAME","AU_Type","BeneficialUse","BEN_CLASS","CharacteristicName","ParameterGroupName",
													"CAS", "RuleParameterName", "R3172ParameterName","IR_Value","IR_Unit","ResultMeasure.MeasureUnitCode","IR_DetCond","ResultSampleFractionText","IR_Fraction","CriterionUnits","TargetFraction",
													"IR_LowerLimitValue","IR_LowerLimitUnit","IR_UpperLimitValue","IR_UpperLimitUnit",
													"DataLoggerLine","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode",
													"AssessmentType","TableDescription","CriterionLabel","CriterionType","ParameterQualifier", "FrequencyCombined", "FrequencyNumber", "FrequencyUnit","TargetActivityType",
													"DailyAggFun","AsmntAggPeriod","AsmntAggPeriodUnit","AsmntAggFun","NumericCriterion","SSC_StartMon","SSC_EndMon","SSC_MLID",
													"IR_Site_FLAG","IR_ActMedia_FLAG","IR_LabAct_FLAG","IR_DetCond_FLAG","IR_Unit_FLAG","IR_Parameter_FLAG","IR_DataPrep_FLAG"
													)

### Upload export translation workbook
exp_file=system.file("extdata", "IR_export_translations.xlsx", package = "irTools")
exp_wb = openxlsx::loadWorkbook(exp_file)

# Read in columns from translation workbook
columns = openxlsx::readWorkbook(exp_wb, sheet = 1)
colnames_exp = columns$COL_KEEP[columns$SHEET=="DA"]

reasons=data.frame(data[0,])
reasons$reason=character(0)


#Remove records w/o criteria (in case they have been optionally passed through assignCriteria - these cause errors in aggregation steps as they do not have aggregation functions specified)
count=length(data$CriterionUnits[is.na(data$CriterionUnits)])
if(count>0){warning(paste(count, 'records being removed due to lack of criteria & units in standards table. These may have been purposely passed through in assign criteria.'))}
data=data[!is.na(data$CriterionUnits),]# | data$BeneficialUse=="CF" | data$R3172ParameterName=="Profile depth",]

table(data$BeneficialUse)

####################################
######Activity type check###########
####################################
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[IR_ActivityType!=TargetActivityType & BeneficialUse!='SUP']="Non-assessed activity type for parameter"
	})
data_n=data_n[!is.na(data_n$reason),]
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
table(reasons$BeneficialUse)
#print(table(reasons$reason))
rm(data_n)


####################################
######Result value present check####
####################################
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[is.na(IR_Value)]="No result value or detection limit"
	})
data_n=data_n[!is.na(data_n$reason),]
reasons=rbind(reasons, data_n[!is.na(data_n$reason),])
table(reasons$BeneficialUse)
#print(table(reasons$reason))
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
table(reasons$BeneficialUse)
#print(table(reasons$reason))
rm(data_n)

with(subset(reasons, reason=='Non-assessed fraction or fraction not defined, & fraction specified by criterion'), {table(ResultSampleFractionText, TargetFraction)})



##################################
###Unit conversion table checks###
##################################

#Read unit conversion table from translation workbook (loaded above)
unit_convs=data.frame(openxlsx::readWorkbook(translation_wb, sheet=unit_sheetname, startRow=startRow_unit))

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
	table(reasons$BeneficialUse)
	#print(table(reasons$reason))
}

##################################
###Unit conversions for IR data###
##################################

#Double check that blanks are all NA in data (shouldn't really need this at this point)
# data[data==""]=NA

#Merge conversion table to data
data=merge(data,unit_convs,all.x=T)
dim(data)

#Manually convert deg F to C
data=within(data, {
	IR_Value=ifelse(IR_Unit=='deg F' & CriterionUnits=='C',  (IR_Value-32)*5/9, IR_Value)
	IR_Unit=ifelse(IR_Unit=='deg F' & CriterionUnits=='C',  'deg C', IR_Unit)
})

#Reject records where IR_UnitConv_FLAG is REJECT
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[IR_UnitConv_FLAG=="REJECT"]="Inappropriate units for conversion or assessment"
	})
reasons=plyr::rbind.fill(reasons, data_n[!is.na(data_n$reason),])
table(reasons$BeneficialUse)
#print(table(reasons$reason))
rm(data_n)

##Reject records with pH<=0 or pH>16
#data_n=data
#data_n$reason=NA
#data_n=within(data_n,{
#	reason[IRParameterName=="pH" & (IR_Value<=0 | IR_Value>16)]="Reported pH value <=0 or >16"
#	})
#reasons=plyr::rbind.fill(reasons, data_n[!is.na(data_n$reason),])
##print(table(reasons$reason))
#rm(data_n)
#
##Reject records with temperature<=-10 or >50
#data_n=data
#data_n$reason=NA
#data_n=within(data_n,{
#	reason[IRParameterName=="Max. Temperature" & (IR_Value<-10 | IR_Value>40)]="Reported temperature value <-10 or >40"
#	})
#reasons=plyr::rbind.fill(reasons, data_n[!is.na(data_n$reason),])
##print(table(reasons$reason))
#rm(data_n)

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

# Reject records non-detect records when a detection is available on the same day
nd=data[data$IR_DetCond=='ND',]
det=unique(data[data$IR_DetCond=='DET', c('ActivityIdentifier', 'IR_MLID', 'R3172ParameterName', 'IR_Fraction', 'ActivityStartDate')])
det$reason='Non detect value with a detection value available for this activity identifier, parameter, and fraction'
nd=merge(nd, det)
nd=nd[!is.na(nd$reason),names(nd) %in% names(reasons)]
reasons=rbind(reasons, nd[!is.na(nd$reason),])
table(reasons$BeneficialUse)
print(table(reasons$reason))
rm(nd)

# Assign correction factors to data requiring calculations
if(any(data$BeneficialUse=="CF")){
	## Extract CFs
	cfs=unique(data[data$BeneficialUse=="CF",c("ActivityStartDate","IR_MLID","R3172ParameterName","DailyAggFun","IR_Unit","IR_Value")])
	#data=data[data$BeneficialUse!="CF",]
	calcs=data[which(data$NumericCriterion=="calc"),]
	dim(calcs)
	data=data[is.na(data$NumericCriterion) | data$NumericCriterion!="calc",]
	
	## Aggregate CFs to daily values & cast to wide format
	drop_vars=""
	cfs_daily=aggDVbyfun(cfs, drop_vars=drop_vars, value_var="IR_Value", agg_var="DailyAggFun")
	
	dim(cfs)
	dim(cfs_daily)
	
	cfs_daily$cf=paste0("cf_",cfs_daily$DailyAggFun,"_",cfs_daily$R3172ParameterName,"_",cfs_daily$IR_Unit)
	cfs_daily_cast=reshape2::dcast(cfs_daily, ActivityStartDate+IR_MLID~cf, value.var="IR_Value")
	
	
	## Calculate hardness
	cfs_daily_cast=within(cfs_daily_cast,{
			hardness=100*(`cf_min_Calcium_mg/l`/40.08 + `cf_min_Magnesium_mg/l`/24.3)
			hardness[is.na(hardness)]=`cf_min_Hardness_mg/l`[is.na(hardness)]
			hardness=ifelse(hardness>400,400,hardness) 
		})
	
	## Merge CFs back to data
	dimcheck=dim(calcs)[1]
	calcs=merge(calcs,cfs_daily_cast,all.x=T)
	if(dim(calcs)[1] != dimcheck){
		stop('ERROR: Failure assigning correction factors to data.')
	}
	
	
	## Calculate CF dependent criteria
	### Load criterion workbook
	criterion_wb=openxlsx::loadWorkbook(crit_wb)
	
	### Remove filters from all sheets in crit_wb
	sheetnames=openxlsx::getSheetNames(crit_wb)
	for(n in 1:length(sheetnames)){
	openxlsx::removeFilter(criterion_wb, sheetnames[n])
	}
	
	### Read formula table
	cf_formulas=data.frame(openxlsx::readWorkbook(criterion_wb, sheet=cf_formulas_sheetname, startRow=startRow_formulas))
	# ssc_cf_formulas = subset(cf_formulas, !is.na(cf_formulas$ss_R317Descrp))
	cf_formulas=unique(cf_formulas[,names(cf_formulas) %in% c("CAS","BeneficialUse","TableDescription","FrequencyNumber","FrequencyUnit","CF","CriterionFormula","ParameterQualifier","CriterionUnits", "SS_calc")])
	names(calcs)[names(calcs) %in% names(cf_formulas)]
	
	### Merge formulas to data
	dimcheck=dim(calcs)[1]
	calcs = merge(calcs, cf_formulas, all.x = TRUE) 
	if(dim(calcs)[1] != dimcheck){
		stop("ERROR assiging formulas to parameters with calculated criteria (1).")
	}
	
	table(calcs$R3172ParameterName, calcs$CriterionFormula)[rowSums(table(calcs$R3172ParameterName, calcs$CriterionFormula))>0,]
	if(any(is.na(calcs$CriterionFormula))){
		stop("ERROR assiging formulas to parameters with calculated criteria (2).")
	}
	
	
	
	### Fill & evaluate formula
	calcs=within(calcs, {
		CF[!is.na(CF)]=paste0("(",CF[!is.na(CF)],")")
		CriterionFormula=gsub("CF * ","",CriterionFormula, fixed=TRUE)
		CriterionFormula[!is.na(CF)]=paste0("(",CriterionFormula[!is.na(CF)],")")
		CriterionFormula[!is.na(CF)]=paste(CF[!is.na(CF)], "*", CriterionFormula[!is.na(CF)])
		CriterionFormula=gsub("MIN", "min", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("MAX", "max", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("ln", "log", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub(") (", ")*(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub(")(",  ")*(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("(e(",  "(exp(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("e(",  "exp(", CriterionFormula, fixed=TRUE)
		CriterionFormula=gsub("IFELSE",  "ifelse", CriterionFormula, fixed=TRUE)
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "hardness", as.character(hardness))
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "min_pH", as.character(`cf_min_pH_pH units`))
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "max_pH", as.character(`cf_max_pH_pH units`))
		CriterionFormula=stringr::str_replace_all(CriterionFormula, "T", as.character(`cf_max_Max. Temperature_C`))
		CalculatedCrit=sapply(CriterionFormula, function(x) eval(parse(text=x)))
		suppressWarnings({
			NumericCriterion=wqTools::facToNum(NumericCriterion)
		})
		NumericCriterion=CalculatedCrit
	})
	
	#plot(calcs$CalculatedCrit~calcs$hardness)
	#plot(calcs$CalculatedCrit~calcs$`cf_max_pH_pH units`)
	
	### Bind calculated criteria data back to full dataset
	#calcs=calcs[,unique(c(col_names,'cf_max_Max. Temperature_C','cf_max_pH_pH units','cf_min_Calcium_mg/l','cf_min_Hardness_mg/l','cf_min_Magnesium_mg/l','cf_min_pH_pH units','hardness','CriterionFormula','CalculatedCrit'))]	
	
	dim(calcs)
	dim(data)
	
	data=plyr::rbind.fill(data, calcs)
	table(data$BeneficialUse)
}	

#Reject records where a criterion could not be calculated
data_n=data
data_n$reason=NA
data_n=within(data_n,{
	reason[R3172ParameterName!='Aluminum' & is.na(NumericCriterion) & BeneficialUse!='SUP' & BeneficialUse!='CF']="Missing one or more correction factors, unable to calculate criterion"
	reason[R3172ParameterName=='Aluminum' & is.na(NumericCriterion) & BeneficialUse!='SUP' & BeneficialUse!='CF']="Missing one or more correction factors, unable to calculate criterion, or chronic Aluminum criterion not applicable."
	})
reasons=plyr::rbind.fill(reasons, data_n[!is.na(data_n$reason),])
table(reasons$BeneficialUse)
#print(table(reasons$reason))
rm(data_n)


#Reject non-detect records where IR_LowerLimitValue > NumericCriterion
data_n=data
data_n$reason=NA
suppressWarnings({
	data_n=within(data_n,{
		reason[which(BeneficialUse!='SUP' & BeneficialUse!='CF' & !is.na(NumericCriterion) & IR_DetCond=="ND" & as.numeric(IR_LowerLimitValue)>as.numeric(NumericCriterion))]="Non-detect result with detection limit > criterion"
	})
})
table(data_n$reason)

reasons=plyr::rbind.fill(reasons, data_n[!is.na(data_n$reason),])
table(reasons$BeneficialUse)
#print(table(reasons$reason))
rm(data_n)

####Apply rejections to flag column in data
cols = c("ResultIdentifier","BeneficialUse","IR_Fraction","TableDescription","TargetFraction","FrequencyCombined","CriterionLabel","FrequencyNumber","FrequencyUnit","CriteriaQualifier")
flags=unique(reasons[,c(cols,"reason")])
len = dim(flags)[2]
flags = tidyr::pivot_wider(flags, id_cols = all_of(cols), names_from = "reason", values_from = "reason")
flags$IR_DataPrep_COMMENT = do.call(paste, c(flags[,len:dim(flags)[2]],sep=","))
flags = flags[,names(flags)%in%c(cols, "IR_DataPrep_COMMENT")]
flags$IR_DataPrep_FLAG="REJECT"
dimcheck=dim(data)[1]
data=merge(data,flags,all.x=T)
#result$data_flags=data

##### NOTE: Removed this reason for rejecting data, because it was primarily rejecting quality data
# assoc_rej_records=subset(data, IR_DataPrep_FLAG=="REJECT" & !ResultIdentifier %in% reasons$ResultIdentifier)
# assoc_rej_records=assoc_rej_records[,!names(assoc_rej_records) %in% "IR_DataPrep_FLAG"]
# assoc_rej_records$reason="Associated with a rejected record"

# reasons=rbind(reasons, assoc_rej_records)

result$rej_data_reasons=reasons


if(dimcheck!=dim(data)[1]){
	stop("ERROR: Error in applying data prep flags. Data dimension[1] has changed.")
	}
data=within(data, {IR_DataPrep_FLAG[is.na(IR_DataPrep_FLAG)]="ACCEPT"})

print("Data prep record ACCEPT/REJECT counts:")
print(table(data$IR_DataPrep_FLAG))
table(data[data$IR_DataPrep_FLAG=="ACCEPT","IR_UnitConv_FLAG"])



###Pull out accepted data
names(data)=make.names(names(data))
acc_data=data[data$IR_DataPrep_FLAG=="ACCEPT",]
#Remove profiles from acc_data
acc_data=acc_data[is.na(acc_data$DataLoggerLine),]
dim(acc_data)
col_names1 = col_names[col_names%in%names(acc_data)]
result$acc_data=acc_data[,col_names1]
colnames_export = names(acc_data)[names(acc_data)%in%colnames_exp]
result$export_data = acc_data[,colnames_export]
#result$rej_data=data[data$IR_DataPrep_FLAG!="ACCEPT",]
subset(acc_data, IR_MLID=='UTAHDWQ_WQX-5995245' & R3172ParameterName=='Aluminum')


#######
####Extract lake profiles
#result$lake_profiles=acc_data[!is.na(acc_data$DataLoggerLine) & acc_data$BeneficialUse %in% c("3A","3B","3C","3D","3E"),]
#
#table(result$lake_profiles$R3172ParameterName)
#

##Extract lakes trophic data
#result$lakes_trophic=acc_data[acc_data$AU_Type=="Reservoir/Lake" & acc_data$R3172ParameterName %in% c("Chlorophyll a", "Total Phosphorus as P","Depth, Secchi disk depth"),]
#
#Extract e coli
result$ecoli=acc_data[acc_data$R3172ParameterName=="E. coli",]


#############
#######Toxics
######
if(any(acc_data$AssessmentType=="Toxic")){

drop_vars=c("ResultIdentifier","DataLoggerLine","OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ResultMeasure.MeasureUnitCode","ActivityDepthHeightMeasure.MeasureUnitCode", "IR_ActivityType", "TargetActivityType",
			"R317Descrp","IR_DetCond", "MonitoringLocationIdentifier","MonitoringLocationTypeName","IR_LowerLimitValue","IR_LowerLimitUnit","IR_UpperLimitValue","IR_UpperLimitUnit","ResultSampleFractionText","IR_Fraction","CharacteristicName",
			"IR_Site_FLAG","IR_ActMedia_FLAG","IR_LabAct_FLAG","IR_DetCond_FLAG","IR_Unit_FLAG","IR_Parameter_FLAG")

## Extract radium data
result$radium=acc_data[acc_data$R3172ParameterName=='Radium 226, 228 (Combined)',]

toxics_raw=subset(acc_data, AssessmentType=="Toxic" & BeneficialUse!="CF")
toxics_raw=toxics_raw[toxics_raw$R3172ParameterName!='Radium 226, 228 (Combined)',]
toxics_raw=toxics_raw[,col_names]

	#split streams & lakes
	toxics_strms=toxics_raw[which(toxics_raw$AU_Type=="River/Stream"),]
	toxics_lakes=toxics_raw[which(toxics_raw$AU_Type=="Reservoir/Lake"),]
	
	if(dim(toxics_strms)[1]>0){
		###Streams
		#Aggregate to daily values
		dim(toxics_strms)
		toxics_strms_daily=aggDVbyfun(toxics_strms,	drop_vars=drop_vars, value_var="IR_Value", agg_var="DailyAggFun")
		dim(toxics_strms_daily)
	}
	
	
	###Lakes (JV note - applying same logic as streams)
	
	if(dim(toxics_lakes)[1]>0){
		#Aggregate to daily values
		dim(toxics_lakes)
		toxics_lakes_daily=aggDVbyfun(toxics_lakes,	drop_vars=drop_vars, value_var="IR_Value", agg_var="DailyAggFun")
		dim(toxics_lakes_daily)		
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
	
	#Generate toxics result
	result$toxics=toxics	
}

#############
#######Conventionals
######
drop_vars=c("ResultIdentifier","DataLoggerLine","OrganizationIdentifier","ActivityIdentifier", "ActivityStartTime.Time","ActivityRelativeDepthName","ActivityDepthHeightMeasure.MeasureValue","ActivityDepthHeightMeasure.MeasureUnitCode","ResultMeasure.MeasureUnitCode","IR_ActivityType", "TargetActivityType",
			"R317Descrp","IR_DetCond","MonitoringLocationIdentifier","MonitoringLocationTypeName","IR_LowerLimitValue","IR_LowerLimitUnit","IR_UpperLimitValue","IR_UpperLimitUnit","ResultSampleFractionText","CharacteristicName","IR_Fraction",
			'cf_max_Max..Temperature_C','cf_max_pH_pH.units','cf_min_Calcium_mg.l','cf_min_Hardness_mg.l','cf_min_Magnesium_mg.l','cf_min_pH_pH.units','hardness','CriterionFormula','CalculatedCrit',
			"IR_Site_FLAG","IR_ActMedia_FLAG","IR_LabAct_FLAG","IR_DetCond_FLAG","IR_Unit_FLAG","IR_Parameter_FLAG")

if(any(acc_data$AssessmentType=="Conventional")){
	conv_raw=acc_data[which(acc_data$AssessmentType=="Conventional" & acc_data$BeneficialUse!="CF"),]
	conv_raw=conv_raw[,col_names]
	
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
			conv_lakes$ActivityDepthHeightMeasure.MeasureValue=wqTools::facToNum(conv_lakes$ActivityDepthHeightMeasure.MeasureValue)
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
		
		#Select out lakes TDS
		if(dim(conv_lakes_daily)[1]>0){		
			#Separate lakes TDS (different assessment methods)
			result$lakes_tds=conv_lakes_daily[conv_lakes_daily$R3172ParameterName=="Total Dissolved Solids",]
			conv_lakes_daily=conv_lakes_daily[conv_lakes_daily$R3172ParameterName!="Total Dissolved Solids",]
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

#Other possible checks - execution TBD
#Rivers/streams depth check
#Value based flags & rejections (if performing, desired flags/rejections should be input to param translation table)
#Estimated & calculated value check
#Holding times

objects(result)

return(result)

}
