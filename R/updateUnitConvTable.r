#' Update IR unit conversion table
#'
#' Updates IR unit conversion table based on combinations of native and target units as defined by numeric criteria tables.
#'
#' @param data A merged WQP results-activity R-object that has had detection limit values filled (passed through fillMaskedValues function) and numeric criteria assigned (passed through assignCriteria function).
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).
#' @param sheetname Name of sheet in workbook holding IR unit conversion table. Defaults to "unitConvTable".
#' @param startRow Row to start reading the unit conversion excel sheet from (in case headers have been added). Defaults to 1.

#' @return Updates unit conversion table in translation_wb with any new combinations of native and target units.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter

#' @export
updateUnitConvTable=function(data, translation_wb, sheetname="unitConvTable", startRow=1, startCol=1){


######TESTING SETUP
# translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\03translation\\ir_translation_workbook.xlsx"
# data=data_crit
# sheetname="unitConvTable"
# startRow=1
# startCol=1
######


#Load translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	openxlsx::removeFilter(trans_wb, sheetnames[n])
	}


####Unit Conversions
#Identify unique combinations of IR_Unit and CriterionUnits.
rcunits=data.frame(unique(data[,c("IR_Unit", "CriterionUnits")]))
rcunits$InData="Y"
rcunits[rcunits==""]<-NA
rcunits=na.omit(rcunits)

# Load unitConvTable sheet, set all blanks to NA, and reset InData column.
unitconv_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet="unitConvTable", startRow=startRow, detectDates=TRUE))
column_names=names(unitconv_table)
unitconv_table=unitconv_table[,!names(unitconv_table)%in%"InData"]

# Merge NEW unique combinations of ResultMeasure.MeasureUnitCode/CriterionUnits to unitConvTable.
unit_merge=merge(rcunits,unitconv_table,all=T)
unit_merge=unit_merge[,column_names]

# Document date new combination added.
unit_merge$DateAdded[is.na(unit_merge$DateAdded)]=Sys.Date() # this does not work with an empty dataframe
openxlsx::writeData(trans_wb, "unitConvTable", unit_merge, startRow=startRow, startCol=startCol)

# Check to see if new combinations exist and alert user.
new_unitcombo_count=dim(unit_merge)[1]-dim(unitconv_table)[1]
if(new_unitcombo_count>0){
  print(paste("WARNING:",new_unitcombo_count,"new result-criterion unit combination(s) identified. Populate necessary correction factors in unitConvTable."))
  readline(prompt="Press [enter] to continue")
  print("unitConvTable updated.")} else{print("No new result-criterion unit combinations identified.")}

###Save translation workbook
openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")

}



