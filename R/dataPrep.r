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
data=data_crit
translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
unit_sheetname="unitConvTable"
startRow=1

######################################################
###Data prep for dissolved vs. total fraction check###
######################################################

# Reduce dataframe to columns of interest for comparing total and dissolved, narrow down rows to unique values (currently values are duplicated if measurement subject to multiple uses/standards)
unique(data$ResultSampleFractionText)# note that NA's may be in this list.
data1 <- data[,names(data)%in%c("ActivityStartDate","ActivityIdentifier","R3172ParameterName","FractionGroup","IR_Unit", "IR_Value")]
data1 <- unique(data1)

# Separate into TOTAL and DISSOLVED objects, and give unit/value columns unique names specific to total or dissolved
tot <- subset(data1, data1$FractionGroup=="TOTAL")
dim(tot)
tot <- tot[,!names(tot)%in%c("FractionGroup")]
names(tot)[names(tot)=="IR_Value"]<- "IR_Value_Tot"
names(tot)[names(tot)=="IR_Unit"]<- "IR_Unit_Tot"

diss <- subset(data1, data1$FractionGroup=="DISSOLVED")
dim(diss)
diss <- diss[,!names(diss)%in%c("FractionGroup")]
names(diss)[names(diss)=="IR_Value"]<- "IR_Value_Diss"
names(diss)[names(diss)=="IR_Unit"]<- "IR_Unit_Diss"

# Merge TOTAL and DISSOLVED objects based on AID, Start Date, and Parameter name
diss_tot <- merge(tot,diss, by=c("ActivityIdentifier","ActivityStartDate","R3172ParameterName"))

# Find all unique combos of TOTAL and DISSOLVED units to run through updateUnitConvTable function.
unit_combos <- diss_tot[,names(diss_tot)%in%c("IR_Unit_Tot","IR_Unit_Diss")]
#***NOTE***IR_Unit_Diss arbitrarily made TARGET (CriterionUnits) unit.
names(unit_combos)[names(unit_combos)=="IR_Unit_Diss"] <- "CriterionUnits"
names(unit_combos)[names(unit_combos)=="IR_Unit_Tot"] <- "IR_Unit"
updateUnitConvTable(unit_combos, translation_wb, sheetname="unitConvTable", startRow=1, startCol=1)

# Bind unit_combo columns to diss_tot columns (maintain original column names differentiating total with dissolved, but include duplicated columns with unitConvTable column names)
diss_tot <- cbind(diss_tot,unit_combos)

##################################
###Unit conversion table checks###
##################################

#Load translation workbook updated from comparison of TOTAL/DISSOLVED units above.
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
}

#Read unit conversion table 
unit_convs=data.frame(openxlsx::readWorkbook(trans_wb, sheet=unit_sheetname, startRow=startRow, detectDates=TRUE))
unit_convs=subset(unit_convs,unit_convs$InData=="Y")
unit_convs=unit_convs[!names(unit_convs) %in% c("DateAdded","InData")]
unit_convs[unit_convs==""]=NA
names(unit_convs)[names(unit_convs)=="IR_FLAG"]="IR_UnitConv_FLAG"

# Check to make sure all IR_UnitConv_FLAG populated
if(any(is.na(unit_convs$IR_UnitConv_FLAG))){
  print("WARNING: Unit conversion table missing required IR_FLAG information. Please correct the table before proceeding.")
}

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
diss_tot_units$IR_Unit_Tot=diss_tot_units$CriterionUnits

# Set all comparisons to baseline ACCEPT
diss_tot_units$Dis_Tot_FLAG = "ACCEPT"

# Find comparisons where TOTAL > DISSOLVED
diss_tot_units$Dis_Tot_FLAG <- ifelse(diss_tot_units$IR_Value_Tot>diss_tot_units$IR_Value_Diss,"REJECT",diss_tot_units$Dis_Tot_FLAG)

# Merge Dis_Tot_FLAG info back to data.
ds_test <- diss_tot_units[,names(diss_tot_units)%in%c("ActivityIdentifier","ActivityStartDate","R3172ParameterName","Dis_Tot_FLAG")]
data <- merge(data,ds_test, all.x=TRUE)
dim(data)
unique(data$Dis_Tot_FLAG)
table(data$Dis_Tot_FLAG)

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

#Check for NA conversion factors (where units needed)
NAconvcheck <- any(is.na(data$UnitConversionFactor) & !is.na(data$IR_Unit) & !is.na(data$CriterionUnits) & data$IR_UnitConv_FLAG=="ACCEPT")
if(NAconvcheck=="TRUE"){
  print("WARNING: unitConvTable missing conversion factor(s) for accepted IR_Unit/CriterionUnits combination(s)")
}

#When IR_Unit = CriterionUnit, make UnitConversionFactor 1
data$UnitConversionFactor[data$IR_Unit==data$CriterionUnits]=1

#Convert IR_Value using Unit Conversion Value
data$IR_Value <- data$IR_Value*data$UnitConversionFactor
data$IR_Unit = data$CriterionUnits

#Aggregate to daily values

#Activity type check

#Fraction type check

#Other possible checks - execution TBD
#Rivers/streams depth check
#Value based flags & rejections (if performing, desired flags/rejections should be input to param translation table)
#Estimated & calculated value check
#Holding times

data[data$CharacteristicName=="Arsenic",names(data)%in%c("IR_Unit","CriterionUnits","R3172ParameterName","BeneficialUse","IR_Value","UnitConversionFactor","RejectMax","RejectMin", "IR_UnitConv_FLAG")]


}
