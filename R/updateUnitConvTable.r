#' Update IR unit conversion table
#'
#' Updates IR unit conversion table based on combinations of native and target units as defined by numeric criteria tables.
#'
#' @param data A merged WQP results-activity R-object that has had detection limit values filled (passed through fillMaskedValues function) and numeric criteria assigned (passed through assignCriteria function).
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).
#' @param sheetname Name of sheet in workbook holding IR unit conversion table. Defaults to "unitConvTable".
#' @param startRow Row to start reading the unit conversion excel sheet from (in case headers have been added). Defaults to 1.
#' @param startCol Column to start reading the unit conversion excel sheet from (in case columns have been added). Defaults to 1.

#' @return Updates unit conversion table in translation_wb with any new combinations of native and target units.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter

#' @export
updateUnitConvTable=function(data, translation_wb, sheetname="unitConvTable", startRow=1, startCol=1){

######TESTING SETUP
# translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
# #translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\03translation\\ir_translation_workbook.xlsx"
# data=data_crit
# sheetname="unitConvTable"
# startRow=1
# startCol=1
# ######
# 
# 
# #Load translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	openxlsx::removeFilter(trans_wb, sheetnames[n])
	}

#######Unit Conversions######
##TOTAL versus DISSOLVED Fraction Units
# Reduce dataframe to columns of interest for comparing total and dissolved, narrow down rows to unique values (currently values are duplicated if measurement subject to multiple uses/standards)
unique(data$ResultSampleFractionText)# note that NA's may be in this list.
data1 <- data[,names(data)%in%c("ActivityStartDate","ActivityIdentifier","R3172ParameterName","FractionGroup","IR_Unit", "IR_Value")]
data1 <- unique(data1)

# Separate into TOTAL and DISSOLVED objects, and give unit/value columns unique names specific to total or dissolved
#NOTE:DISSOLVED IR_Unit arbitrarily made TARGET (CriterionUnits) unit.
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

# Find all unique combos of TOTAL and DISSOLVED units to run through updateUnitConvTable function.
dtunits <- data.frame(unique(diss_tot[,c("IR_Unit","CriterionUnits")]))

##IR_Unit versus CriterionUnits
#Identify unique combinations of IR_Unit and CriterionUnits.
rcunits=data.frame(unique(data[,c("IR_Unit", "CriterionUnits")]))
#rcunits=na.omit(rcunits)

##Combine these unit conversions into one dataframe
all_unit_combos <- rbind(dtunits,rcunits)
all_unit_combos <- unique(all_unit_combos[!is.na(all_unit_combos$CriterionUnits),])
all_unit_combos$InData="Y"
all_unit_combos[all_unit_combos==""]<-NA

# Load unitConvTable sheet, set all blanks to NA, and reset InData column.
unitconv_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE))
column_names=names(unitconv_table)
unitconv_table=unitconv_table[,!names(unitconv_table)%in%"InData"]

# Merge NEW unique combinations of ResultMeasure.MeasureUnitCode/CriterionUnits to unitConvTable.
unit_merge=merge(all_unit_combos,unitconv_table,all=T)
unit_merge=unit_merge[,column_names]

# Document date new combination added.
unit_merge$DateAdded[is.na(unit_merge$DateAdded)]=Sys.Date() # this does not work with an empty dataframe
openxlsx::writeData(trans_wb, "unitConvTable", unit_merge, startRow=startRow, startCol=startCol)

###Save translation workbook
openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")

# Check to see if new combinations exist and alert user.
new_unitcombo_count=dim(unit_merge)[1]-dim(unitconv_table)[1]
if(new_unitcombo_count>0){
  print(paste("WARNING:",new_unitcombo_count,"new result-criterion unit combination(s) identified. Populate necessary correction factors in unitConvTable. Be aware that some combinations may be associated with pH (unitless)"))
  readline(prompt="Press [enter] to continue")
  print("unitConvTable updated.")} else{print("No new result-criterion unit combinations identified.")}

}



