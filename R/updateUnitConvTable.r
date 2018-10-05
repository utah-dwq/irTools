#' Update IR unit conversion table
#'
#' Updates IR unit conversion table based on combinations of native and target units as defined by numberic criteria tables.
#'
#' @param data A merged, translated, and numeric criteria assigned WQP results R-object.
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
updateUnitConvTable=function(data, translation_wb, sheetname="activityCommentTable", startRow=1, startCol=1){


###
###NOTE: Code below is from updateCommentTable. To be used as a template for updateUnitConvTable
###









######TESTING SETUP
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\translationWorkbook\\IR_translation_workbook.xlsx"
#data=merged_results_sub
#sheetname="activityCommentTable"
######


#Load translation workbook
trans_wb=loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	removeFilter(trans_wb, sheetnames[n])
	}


####ActivityCommentText
#Identify unique ActivityCommentText values
comments=data.frame(unique(data[,c("ResultLaboratoryCommentCode","ResultLaboratoryCommentText","ActivityCommentText","ResultCommentText")]))
comments$InData="Y"

comment_table=data.frame(readWorkbook(trans_wb, sheet="activityCommentTable", startRow=startRow, detectDates=TRUE))
column_names=names(comment_table)
comment_table=comment_table[,!names(comment_table)%in%"InData"]

comment_merge=merge(comments,comment_table,all=T)
comment_merge=comment_merge[,column_names]

new_comment_count=dim(comment_merge)[1]-dim(comment_table)[1]
comment_merge$DateAdded[is.na(comment_merge$DateAdded)]=Sys.Date()
writeData(trans_wb, "activityCommentTable", comment_merge, startRow=startRow, startCol=startCol)

print(paste("activityCommentTable updated.", new_comment_count,"new ActivityCommentText values identified."))

###Save translation workbook
saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")


}



