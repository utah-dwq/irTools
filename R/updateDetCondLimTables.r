#' Update detection condition and limit type tables
#'
#' This function identifies new ResultDetectionConditionText & DetectionQuantitationLimitTypeName values in WQP data for which are not in IR translation tables and appends them into the translation workbook for review.
#' New ResultDetectionConditionText & DetectionQuantitationLimitTypeName values generated by this function are derived from those in detquantlim that match (via merge) records in results input (this allows subsetting of these lists by other previously generated data screens if desired).

#' @param results A WQP results (must include narrow result, merged, wide objects OK) R-object name.
#' @param detquantlim A WQP detection/quantitation limit file R-object. Should be matching complement to WQP results input. New values derived from those that successfully merge to results input.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx). 

#' @param detConditionTable_sheetname Name of sheet in workbook holding result detection condition names table. Defaults to "detConditionTable".
#' @param detConditionTable_startRow Row to start reading the detConditionTable excel sheet from (in case headers have been added). Defaults to 3.
#' @param detConditionTable_startCol Col to start writing the detConditionTable excel sheet to (to the right of all formula based columns). Defaults to 1.

#' @param detLimitTypeTable_sheetname  Name of sheet in workbook holding detection limit type names and ranked prioritizations table. Defaults to "detLimitTypeTable".
#' @param detLimitTypeTable_startRow Row to start reading the detLimitTypeTable excel sheet from (in case headers have been added). Defaults to 3.
#' @param detLimitTypeTable_startCol Col to start writing the detLimitTypeTable excel sheet to (to the right of all formula based columns). Defaults to 1.

#' @return Appends any new values in WQP data to translation_wb for review. This updates the input translation_wb with those new rows.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames


#' @export
updateDetCondLimTables=function(results,detquantlim,translation_wb,
								detConditionTable_sheetname="detConditionTable", detConditionTable_startRow=3, detConditionTable_startCol=1,
								detLimitTypeTable_sheetname="detLimitTypeTable", detLimitTypeTable_startRow=3, detLimitTypeTable_startCol=1
								){

####TESTING SETUP
####
#
#results=merged_results
#detquantlim=detquantlim
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\translationWorkbook\\ir_translation_workbook.xlsx"
#detConditionTable_sheetname="detConditionTable"
#detConditionTable_startRow=3
#detConditionTable_startCol=1
#detLimitTypeTable_sheetname="detLimitTypeTable"
#detLimitTypeTable_startRow=3
#detLimitTypeTable_startCol=1
#######
#######


#Load translation workbook
trans_wb=loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	removeFilter(trans_wb, sheetnames[n])
	}



###Merge detquantlim to results	(inner)
res_dql=merge(results, detquantlim)


#detConditionTable
detconds=data.frame(unique(res_dql[,c("ResultDetectionConditionText")]))
names(detconds)="ResultDetectionConditionText"
detconds[detconds==""]=NA
detconds$InData="Y"

detConditionTable=data.frame(readWorkbook(trans_wb, sheet=detConditionTable_sheetname, startRow=detConditionTable_startRow, detectDates=TRUE))
detConditionTable_names=names(detConditionTable)
detConditionTable=detConditionTable[,!names(detConditionTable)%in%"InData"]

detcond_merge=merge(detconds, detConditionTable, all=T)

detcond_merge=detcond_merge[,detConditionTable_names]

writeData(trans_wb, detConditionTable_sheetname, detcond_merge, startRow=detConditionTable_startRow+1, startCol=detConditionTable_startCol,colNames=F)

# Check for new detection condition text
detcondstext <- detconds$ResultDetectionConditionText
trans_wb_detconds <- detConditionTable$ResultDetectionConditionText
new_det_conds <- detcondstext[!detcondstext%in%trans_wb_detconds]

if(length(new_det_conds)>0){
  print("WARNING: New ResultDetectionConditionText value(s) identified")
  print(data.frame(new_det_conds))
  readline(prompt="Press [enter] to continue")
  print("detConditionTable updated.")} else{print("No new ResultDetectionConditionText value(s) identified")}


#detLimitTypeTable	
detlims=data.frame(unique(res_dql[,c("DetectionQuantitationLimitTypeName")]))
names(detlims)="DetectionQuantitationLimitTypeName"
detlims[detlims==""]=NA
detlims$InData="Y"

detLimitTypeTable=data.frame(readWorkbook(trans_wb, sheet=detLimitTypeTable_sheetname, startRow=detLimitTypeTable_startRow, detectDates=TRUE))
detLimitTypeTable_names=names(detLimitTypeTable)
detLimitTypeTable=detLimitTypeTable[,!names(detLimitTypeTable)%in%"InData"]

detlim_merge=merge(detlims, detLimitTypeTable, all=T)

detlim_merge=detlim_merge[,detLimitTypeTable_names]

writeData(trans_wb, detLimitTypeTable_sheetname, detlim_merge, startRow=detLimitTypeTable_startRow+1, startCol=detLimitTypeTable_startCol,colNames=F)

# Check for new detection limit types
detlimtypes <- detlims$DetectionQuantitationLimitTypeName
trans_wb_detlimtypes <- detLimitTypeTable$DetectionQuantitationLimitTypeName
new_det_lim_types <- detlimtypes[!detlimtypes%in%trans_wb_detlimtypes]

if(length(new_det_lim_types)>0){
  print("WARNING: New DetectionQuantitationLimitTypeName value(s) identified")
  print(data.frame(new_det_lim_types))
  readline(prompt="Press [enter] to continue")
  print("detLimitTypeTable updated.")} else{print("No new DetectionQuantitationLimitTypeName value(s) identified.")}

#Save translation wb
saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")


}



