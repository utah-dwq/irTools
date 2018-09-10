#' Apply screens to WQP data by selected table
#'
#' Joins activity review inputs (labNameActivityTable, activityMediaNameTable, & activityCommentTable) to WQP data to apply decisions from input files to data.
#' @param data Either a merged result object (narrowresult<-activity<-quantdetlim) or the full path and filename to a merged result .csv file. If data input is a path/file name, restype must be specified as "filepath".
#' @param datatype Data input type (one of "object" or "filepath"). Defaults to "object". Specify "filepath" to input a path and file name for the results function argument.
#' @param translation_wb Full path and filename for IR translation workbook
#' @param sheetname Name of sheet in workbook holding desired screening decisions
#' @param flag_col_name Name to rename IR_FLAG column to.
#' @param com_col_name Name to rename IR_COMMENT column to.
#' @param startRow Row to start reading excel sheet from (in case additional headers have been added). Defaults to 1.
#' @param na_err Logical. If TRUE (default), exit function with error if any IR_FLAG values are NA. Set to FALSE to apply a screen table without checking for NA values in IR_FLAG.
#' @return A data.frame object of WQP data with merged columns from input screening tables.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @export
applyScreenTable=function(data, datatype="object", translation_wb, sheetname, flag_col_name, com_col_name, startRow=1, na_err=TRUE){ 

##Testing setup
#data=merged_results
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\translationWorkbook\\ir_translation_workbook.xlsx"
#sheetname="labNameActivityTable"
#flag_col_name="IR_LabAct_FLAG"
#com_col_name="LabAct_COMMENT"
#datatype="object"
#startRow=2

# Read in input data
if(datatype=="filepath" & class(data)!="character"){
	stop("datatype specified as filepath, but data input argument is not a filepath")
}
if(datatype=="filepath" & class(data)=="character"){
	data=read.csv(data)
}
if(datatype=="object" & class(data)=="character"){
	stop("datatype specified as object, but data input argument is of class 'character'")
}

#Load workbook
trans_wb=loadWorkbook(translation_wb)

#Read selected sheet in workbook table
screen_table=data.frame(readWorkbook(trans_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE))

#Check for blanks/NAs in screen_table, exit w/ error if present (if na_err==TRUE)
if(na_err==TRUE){
	if(any(is.na(screen_table$IR_FLAG))==TRUE){
		stop("Error: Screen table incomplete and cannot be applied. All records must have IR_FLAG filled in as either ACCEPT or REJECT...")}
}
	
#Remove excess columns (if they exist)
screen_table=screen_table[,!names(screen_table) %in% c("InData","DateAdded", "", "", "", "")]

#Rename IR_FLAG & IR_COMMENT columns to flag_col_name & com_col_name
names(screen_table)[names(screen_table)=="IR_FLAG"]=flag_col_name
names(screen_table)[names(screen_table)=="IR_COMMENT"]=com_col_name

#Set blanks in data to NA for merge
data[data==""]=NA 

#Merge data and screen_table
data_screen=merge(data, screen_table, all.x=T)

#Check that deimension[1] has stayed consistent
if(dim(data_screen)[1]!=dim(data)[1]){
	stop("Error: dimension[1] of input != dimension[1] of output. This is usually a startRow input arg error...")
}

return(data_screen)


}






