#' Identify New WQP LaboratoryName/ActivityTypeCode combinations & ActivityMediaName/ActivityMediaSubdivisionName combinations and append them into IR Parameter Translation Workbook (sheet names labNameActivityTable & activityMediaNameTable)
#'
#' This function identifies new LaboratoryName/ActivityTypeCode combinations & ActivityMediaName/ActivityMediaSubdivisionName combinations in WQP data for which an IR_FLAG has not yet been defined and appends them into the translation workbook for review.
#'
#' @param data A WQP results (must include activity, merged objects OK) R-object or the full path and filename to a WQP result file (.csv). If input is a path/file name, datatype must be specified as "filepath".
#' @param datatype Data input type (one of "object" or "filepath"). Defaults to "object". Specify "filepath" to input a path and file name for the data function argument.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx). 

#' @param labNameActivityTable_sheetname Name of sheet in workbook holding lab name / activity table. Defaults to "labNameActivityTable".
#' @param labNameActivityTable_startRow Row to start reading the labNameActivityTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param labNameActivityTable_startCol Col to start writing the labNameActivityTable excel sheet to (to the right of all formula based columns). Defaults to 3.

#' @param activityMediaNameTable_sheetname  Name of sheet in workbook holding activity / media/submedia name table. Defaults to "activityMediaNameTable".
#' @param activityMediaNameTable_startRow Row to start reading the activityMediaNameTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param activityMediaNameTable_startCol Col to start writing the activityMediaNameTable excel sheet to (to the right of all formula based columns). Defaults to 3.

#' @param fun_cols Vector of names of columns in any table that contain functions. These are excluded from reading and writing by this function.

#' @return Appends any new combinations in WQP data to translation_wb for review. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames

#' @export
updateLabActMediaTables=function(data, datatype="object", translation_wb,
								labNameActivityTable_sheetname="labNameActivityTable",labNameActivityTable_startCol=1,labNameActivityTable_startRow=1,
								activityMediaNameTable_sheetname="activityMediaNameTable",activityMediaNameTable_startCol=1,activityMediaNameTable_startRow=1,
								fun_cols=c()){

######TESTING SETUP
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\translationWorkbook\\IR_translation_workbook.xlsx"
#data=merged_results
#datatype="object"
#labNameActivityTable_sheetname="labNameActivityTable"
#labNameActivityTable_startCol=1
#labNameActivityTable_startRow=1
#activityMediaNameTable_sheetname="activityMediaNameTable"
#activityMediaNameTable_startCol=1
#activityMediaNameTable_startRow=1

#####

#Checking input types
if(datatype=="filepath" & class(data)!="character"){
	stop("datatype specified as filepath, but data argument is not a filepath")
}
if(datatype=="filepath" & class(data)=="character"){
	data=read.csv(data)
}
if(datatype=="object" & class(data)=="character"){
	stop("datatype specified as object, but data argument is of class 'character'")
}

#Load translation workbook
trans_wb=loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	removeFilter(trans_wb, sheetnames[n])
	}


####LaboratoryName/ActivityTypeCode

#Identifying unique labs/activities in input data
labs=data.frame(unique(data[,c("LaboratoryName","LaboratoryAccreditationIndicator","ActivityTypeCode")]))
labs[labs==""]=NA
labs$InData="Y"

lab_table=data.frame(readWorkbook(trans_wb, sheet=labNameActivityTable_sheetname, startRow=labNameActivityTable_startRow, detectDates=TRUE))
column_names=names(lab_table)
lab_table=lab_table[,!names(lab_table)%in%"InData"]

lab_merge=merge(labs, lab_table, all=T)

lab_merge=lab_merge[,column_names]

new_labs_count=dim(lab_merge)[1]-dim(lab_table)[1]
lab_merge$DateAdded[is.na(lab_merge$DateAdded)]=Sys.Date()
writeData(trans_wb, labNameActivityTable_sheetname, lab_merge, startRow=labNameActivityTable_startRow, startCol=labNameActivityTable_startCol)
print(paste("labNameActivityTable updated.", new_labs_count,"new lab/activity combinations identified."))


####ActivityMediaName/ActivityMediaSubdivisionName

#Identify unique ActivityMediaName/ActivityMediaSubdivisionName combinations
media=data.frame(unique(data[,c("ActivityMediaName","ActivityMediaSubdivisionName")]))
media[media==""]=NA
media$InData="Y"

media_table=data.frame(readWorkbook(trans_wb, sheet=activityMediaNameTable_sheetname, startRow=activityMediaNameTable_startRow, detectDates=TRUE))
column_names=names(media_table)
media_table=media_table[,!names(media_table)%in%"InData"]

media_merge=merge(media,media_table,all=T)
media_merge=media_merge[,column_names]

new_media_count=dim(media_merge)[1]-dim(media_table)[1]
media_merge$DateAdded[is.na(media_merge$DateAdded)]=Sys.Date()
writeData(trans_wb, activityMediaNameTable_sheetname, media_merge, startRow=activityMediaNameTable_startRow, startCol=activityMediaNameTable_startCol)
print(paste("activityMediaNameTable updated.", new_media_count,"new media/submedia identified."))


###Save translation workbook
saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")

}
