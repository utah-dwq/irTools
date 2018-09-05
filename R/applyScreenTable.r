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
#' @return A data.frame object of WQP data with merged columns from input screening tables.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @export
applyScreenTable=function(data, datatype="object", translation_wb, sheetname, flag_col_name, com_col_name, startRow=1){ 

##Testing setup
#data=merged_results
#translation_wb="XXXXXXX P:\\WQ\\Integrated Report\\Automation_Development\\jake\\04param_translation\\paramTransTable_v7.xlsx"
#sheetname="masterSiteTable"
#flag_col_name="IR_Site_FLAG"
#com_col_name="IR_Site_COMMENT"
#datatype="object"
#startRow=1

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

#Remove excess columns (if they exist)
screen_table=screen_table[,!names(screen_table) %in% c("InData","DateAdded", "", "", "", "")]

#Rename IR_FLAG & IR_COMMENT columns to flag_col_name & com_col_name
names(screen_table)[names(screen_table)=="IR_FLAG"]=flag_col_name
names(screen_table)[names(screen_table)=="IR_COMMENT"]=com_col_name

#Set blanks in data to NA for merge
data[data==""]=NA 

#Merge data and screen_table
data_screen=merge(data, screen_table, all.x=T)

return(data_screen)


}


