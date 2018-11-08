#' Apply screens to WQP data by selected table
#'
#' Joins activity review inputs (labNameActivityTable, activityMediaNameTable, & activityCommentTable) to WQP data to apply decisions from input files to data.
#' @param data A merged WQP result object. Must include both narrowresult & activity files. May also be a post-fillMaskedValues() results object.
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
applyScreenTable=function(data, translation_wb, sheetname, flag_col_name, com_col_name, startRow=1, na_err=TRUE){ 

##Testing setup
# data=merged_results
# translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
# sheetname="masterSiteTable"
# flag_col_name="IR_Site_FLAG"
# com_col_name="IR_Site_COMMENT"
# startRow=1
# na_err=TRUE

#Load workbook
trans_wb=loadWorkbook(translation_wb)

#Read selected sheet in workbook table
screen_table=data.frame(readWorkbook(trans_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE))

#Check for duplicated combination values in domain table and stop if duplicates detected.
st_dup_check <- screen_table[,!names(screen_table)%in%c("InData","DateAdded","IR_FLAG","IR_COMMENT")] # remove columns containing metadata that could be different between duplicate combinations.
if(any(duplicated(st_dup_check))){
  stop(paste("Duplicated combinations exist in",sheetname,"of translation workbook. Remove duplicates before proceeding."))
}

#Check for blanks/NAs in screen_table, exit w/ error if present (if na_err==TRUE) - JV note - consider adding check for InData so that this only applies to translations in the current dataset
if(na_err==TRUE){
	st_NA_check <- subset(screen_table, screen_table$InData=="Y")
  if(any(is.na(st_NA_check$IR_FLAG))){
		stop("Screen table incomplete and cannot be applied. All records must have IR_FLAG filled in as either ACCEPT or REJECT...")}
}
	
#Remove excess columns (if they exist, could feed additional columns to remove from join at this step)
screen_table=screen_table[,!names(screen_table) %in% c("InData","DateAdded")]

#Rename IR_FLAG & IR_COMMENT columns to flag_col_name & com_col_name
names(screen_table)[names(screen_table)=="IR_FLAG"]=flag_col_name
names(screen_table)[names(screen_table)=="IR_COMMENT"]=com_col_name

#Set blanks in data to NA for merge
data[data==""]=NA 

#Merge data and screen_table
data_screen=merge(data, screen_table, all.x=T)

#Check that merged_result data frame does not contain combinations not present in domain tables
if(any(is.na(data_screen[,names(data_screen)%in%c(flag_col_name)]))){
  stop(paste0("Combinations exist in merged_results that do not exist in ",sheetname,". NA's generated in ",flag_col_name,". Update domain table via update function before proceeding."))
}

#Check that deimension[1] has stayed consistent
if(dim(data_screen)[1]!=dim(data)[1]){
	stop("Error: dimension[1] of input != dimension[1] of output. This is usually a startRow input arg error...")
}

return(data_screen)


}






