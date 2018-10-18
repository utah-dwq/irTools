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
translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\03translation\\ir_translation_workbook.xlsx"
unit_sheetname="unitConvTable"
startRow=1


#Load translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	openxlsx::removeFilter(trans_wb, sheetnames[n])
	}


###Unit conversions

#Read unit conversion table
unit_convs=data.frame(openxlsx::readWorkbook(trans_wb, sheet=unit_sheetname, startRow=startRow, detectDates=TRUE))
unit_convs=unit_convs[!names(unit_convs) %in% "DateAdded"]
unit_convs[unit_convs==""]=NA
names(unit_convs)[names(unit_convs)=="IR_FLAG"]="IR_UnitConv_FLAG"

#Double check that blanks are all NA in data (shouldn't really need this at this point)
data[data==""]=NA

#merge conversion table to data
data=merge(data,unit_convs,all.x=T)

#Check for NA conversion factors (where units needed)
any(is.na(data$UnitConversionFactor) & !is.na(data$IR_Unit) & !is.na(data$CriterionUnits) & data$IR_UnitConv_FLAG=="ACCEPT")

#Set conversion factor to 1 where both IR_Unit & criterion units are NA (e.g. pH)

#Dissolved vs. total fraction check

#Aggregate to daily values

#Activity type check

#Fraction type check

#Other possible checks - execution TBD
#Rivers/streams depth check
#Value based flags & rejections (if performing, desired flags/rejections should be input to param translation table)
#Estimated & calculated value check
#Holding times


}
