#' Identify New WQP comment combinations and append them into IR Parameter Translation Workbook for review (sheet name activityCommentTable)
#'
#' This function identifies new ResultLaboratoryCommentCode/ResultLaboratoryCommentText/ActivityCommentText/ResultCommentText combinations in WQP data for which an IR_FLAG has not yet been defined and appends them into the translation workbook for review.
#'
#' @param data A WQP results (must include activity, merged objects OK) R-object or the full path and filename to a WQP result file (.csv). If input is a path/file name, datatype must be specified as "filepath".
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).
#' @param sheetname Name of sheet in workbook holding comment table. Defaults to "activityCommentTable".
#' @param startRow Row to start reading the activityCommentTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param startCol Col to start writing the activityCommentTable excel sheet to (to the right of all formula based columns). Defaults to 1.

#' @return Appends any new combinations in WQP data to translation_wb for review. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter

#' @export
updateCommentTable=function(data, translation_wb, sheetname="activityCommentTable", startRow=1, startCol=1){

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



