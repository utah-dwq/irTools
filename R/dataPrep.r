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

#' @export
dataPrep=function(data){


#SETUP
#data_crit <- read.csv("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\data_criteria.csv")
data=data_crit
translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
unit_sheetname="unitConvTable"
startRow=1

data$Data_Prep_FLAG="ACCEPT"

#Load translation workbook updated from comparison of TOTAL/DISSOLVED units above.
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
}

####################################
######Activity type check###########
####################################
### EH #### Changed my translation workbook IR_ActivityType(s) to match ParamMeasureType values
data$Data_Prep_FLAG = ifelse(data$IR_ActivityType!=data$ParamMeasureType,"REJECT",data$Data_Prep_FLAG)
print(table(data$Data_Prep_FLAG))

####################################
######Fraction type check###########
####################################
### EH ### Still need to change the FractionGroup header in the col to IR_Fraction.
data$Data_Prep_FLAG = ifelse(data$FractionGroup!=data$TargetFraction,"REJECT",data$Data_Prep_FLAG)
unique(data$Data_Prep_FLAG)
print(table(data$Data_Prep_FLAG))
if(table(data$Data_Prep_FLAG)[1]+table(data$Data_Prep_FLAG)[2]!=dim(data)[1]){
  print("WARNING: NAs coerced in Data_Prep_FLAG due to NA's in IR_Fraction or Target Fraction")
}

######################################################
###Data prep for dissolved vs. total fraction check###
######################################################

# Reduce dataframe to columns of interest for comparing total and dissolved, narrow down rows to unique values (currently values are duplicated if measurement subject to multiple uses/standards)
unique(data$ResultSampleFractionText)# note that NA's may be in this list.
data1 <- data[,names(data)%in%c("ActivityStartDate","ActivityIdentifier", "ActivityStartTime.Time", "R3172ParameterName","FractionGroup","IR_Unit", "IR_Value","Data_Prep_FLAG","Data_Prep_REASON")]
data1 <- unique(data1)

# Separate into TOTAL and DISSOLVED objects, and give unit/value columns unique names specific to total or dissolved
#***NOTE***IR_Unit column in tot retains original "IR_Unit", while IR_Unit in diss converted to "CriterionUnits" (consistent with updateUnitConvTable function)
tot <- subset(data1, data1$FractionGroup=="TOTAL")
dim(tot)
tot <- tot[,!names(tot)%in%c("FractionGroup")]
names(tot)[names(tot)=="IR_Value"]<- "IR_Value_Tot"

diss <- subset(data1, data1$FractionGroup=="DISSOLVED")
dim(diss)
diss <- diss[,!names(diss)%in%c("FractionGroup")]
names(diss)[names(diss)=="IR_Value"]<- "IR_Value_Diss"
names(diss)[names(diss)=="IR_Unit"]<- "CriterionUnits"

# Merge TOTAL and DISSOLVED objects based on AID, Start Date, and Parameter name
diss_tot <- merge(tot,diss, by=c("ActivityIdentifier","ActivityStartDate","R3172ParameterName"))
dim(diss_tot)

##################################
###Unit conversion table checks###
##################################

#Read unit conversion table from translation workbook (loaded above)
unit_convs=data.frame(openxlsx::readWorkbook(trans_wb, sheet=unit_sheetname, startRow=startRow, detectDates=TRUE))

# UNIT CONV TABLE CHECKS: make sure all IR_UnitConv_FLAG and UnitConversionFactor (for ACCEPT combinations) are populated
if(any(is.na(unit_convs$IR_FLAG))){
  stop("Unit conversion table missing required IR_FLAG information. Please correct the table before proceeding.")
}
if(any((is.na(unit_convs$UnitConversionFactor) & unit_convs$IR_FLAG=="ACCEPT")|(is.na(unit_convs$UnitConversionFactor) & is.na(unit_convs$IR_FLAG)))){
  stop("Unit conversion table missing conversion factor(s) for potentially accepted IR_Unit/CriterionUnits combination(s). Please correct the table before proceeding.")
}

unit_convs=subset(unit_convs,unit_convs$InData=="Y")
unit_convs=unit_convs[!names(unit_convs) %in% c("DateAdded","InData")]
unit_convs[unit_convs==""]=NA
names(unit_convs)[names(unit_convs)=="IR_FLAG"]="IR_UnitConv_FLAG"

# Double check that blanks are all NA in data (shouldn't really need this at this point)
diss_tot[diss_tot==""]=NA

# Merge conversion table to data
diss_tot_units=merge(diss_tot,unit_convs,all.x=T)
dim(diss_tot_units)
table(diss_tot_units$IR_UnitConv_FLAG)
diss_tot_units <- diss_tot_units[diss_tot_units$IR_UnitConv_FLAG=="ACCEPT",]
dim(diss_tot_units)

#Convert IR_Unit/Value_Tot to same units as IR_Unit/Value_Diss
diss_tot_units$IR_Value_Tot=diss_tot_units$IR_Value_Tot*diss_tot_units$UnitConversionFactor
diss_tot_units$IR_Unit=diss_tot_units$CriterionUnits

# Find comparisons where DISSOLVED > TOTAL and set Data_Prep_FLAG to REJECT
diss_tot_units$Data_Prep_FLAG = "ACCEPT" # start them all off as ACCEPT
diss_tot_units$Data_Prep_FLAG <- ifelse(diss_tot_units$IR_Value_Tot<diss_tot_units$IR_Value_Diss,"REJECT",diss_tot_units$Data_Prep_FLAG)

# Provide Data_Prep_REASON

# Create dataframe to hold Data_Prep_REASON for REJECTS
# Needs the following columns: AID, RID, ActivityStartDate, R3172ParameterName, Data_Prep_FLAG, Data_Prep_REASON

# Determine rows in diss_tot_units 
diss_tot_units$Data_Prep_REASON = "ACCEPT" # start them all off as ACCEPT
diss_tot_units$Data_Prep_REASON[(diss_tot_units$IR_Value_Tot<diss_tot_units$IR_Value_Diss) & (diss_tot_units$Data_Prep_REASON!="ACCEPT")]<- paste(diss_tot_units$Data_Prep_REASON,"Dissolved fraction greater than total fraction", sep=",") #concatenate multiple reasons
diss_tot_units$Data_Prep_REASON[(diss_tot_units$IR_Value_Tot<diss_tot_units$IR_Value_Diss) & (diss_tot_units$Data_Prep_REASON=="ACCEPT")]<- "Dissolved fraction greater than total fraction"

# # Check to make sure FLAG and REASON reflect same number of changes.
# length(diss_tot_units$Data_Prep_FLAG[diss_tot_units$Data_Prep_FLAG=="REJECT"])
# length(diss_tot_units$Data_Prep_REASON[grepl("Dissolved fraction greater than total fraction",diss_tot_units$Data_Prep_REASON)])

# Merge Dis_Tot_FLAG info back to data.
ds_test <- diss_tot_units[,names(diss_tot_units)%in%c("ActivityIdentifier","ActivityStartDate","R3172ParameterName","Data_Prep_FLAG","Data_Prep_REASON")]
data <- merge(data,ds_test, all=TRUE)
dim(data)
unique(data$Data_Prep_FLAG)
unique(data$Data_Prep_REASON)

##################################
###Unit conversions for IR data###
##################################

#Double check that blanks are all NA in data (shouldn't really need this at this point)
data[data==""]=NA

#Merge conversion table to data (up through this point, same functionality as applyScreenTable)
data=merge(data,unit_convs,all.x=T)
dim(data)

#Retain only rows where IR_UnitConv_FLAG is ACCEPT
table(data$IR_UnitConv_FLAG)
data = data[data$IR_UnitConv_FLAG=="ACCEPT",]
dim(data)

#When IR_Unit = CriterionUnit, make UnitConversionFactor 1
data$UnitConversionFactor[data$IR_Unit==data$CriterionUnits]=1

#Convert IR_Value using Unit Conversion Value
data$IR_Value <- data$IR_Value*data$UnitConversionFactor
data$IR_Unit = data$CriterionUnits

#Aggregate to daily values

#Other possible checks - execution TBD
#Rivers/streams depth check
#Value based flags & rejections (if performing, desired flags/rejections should be input to param translation table)
#Estimated & calculated value check
#Holding times

data[data$CharacteristicName=="Arsenic",names(data)%in%c("IR_Unit","CriterionUnits","R3172ParameterName","BeneficialUse","IR_Value","UnitConversionFactor","RejectMax","RejectMin", "IR_UnitConv_FLAG")]


}
