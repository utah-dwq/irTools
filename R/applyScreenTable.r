#' Apply screens to WQP data by selected table
#'
#' Joins activity review inputs (detConditionTable, labNameActivityTable, activityMediaNameTable, masterSiteTable, paramTransTable, & activityCommentTable) to WQP data to apply decisions from input files to data.
#' @param data A merged WQP result object. Must include both narrowresult & activity files. May also be a post-fillMaskedValues() results object. Note: re-application of edited domain tables to an already screened dataset is not advised, and changes to the domain table likely will not be reflected in a re-screened dataset due to merging conventions.
#' @param wb Full path and filename for Excel workbook containing screen table to be applied
#' @param sheetname Name of sheet in workbook holding desired screening decisions
#' @param flag_col_name Name to rename IR_FLAG column to.
#' @param com_col_name Name to rename IR_COMMENT column to.
#' @param startRow Row to start reading excel sheet from (in case additional headers have been added). Defaults to 1.
#' @param na_dup_err Logical. If TRUE (default), exit function with error if IR_FLAG values are NA or if duplicates detected in combinations in the domain table for which InData=="Y". Set to FALSE to apply a screen table without checking for NA values in IR_FLAG.
#' @return A data.frame object of WQP data with merged columns from input screening tables.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @export
applyScreenTable=function(data, wb, sheetname, flag_col_name, com_col_name, startRow=1, na_dup_err=TRUE){ 

##Testing setup
# data=merged_results_filled
# translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\elise\\demo\\03translation\\ir_translation_workbook.xlsx"
# sheetname="paramTransTable"
# flag_col_name="IR_Parameter_FLAG"
# com_col_name="IR_Parameter_COMMENT"
# startRow=4
# na_dup_err=FALSE

#Load workbook
trans_wb=openxlsx::loadWorkbook(wb)

#Read selected sheet in workbook table
screen_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE), stringsAsFactors=FALSE)

#Check for blanks/NAs and duplicates in screen_table combinations present in data, exit w/ error if present (if na_err==TRUE) 
if(na_dup_err==TRUE){
	st_check <- subset(screen_table, screen_table$InData=="Y")
  if(any(is.na(st_check$IR_FLAG))){
		stop("Screen table incomplete and cannot be applied. All records must have IR_FLAG filled in as either ACCEPT or REJECT...")}
	
	st_check <- st_check[,!names(st_check)%in%c("DateAdded","IR_FLAG","IR_COMMENT")] # remove columns containing metadata that could be different between duplicate combinations.
	if(any(duplicated(st_check))){
	  stop(paste("Duplicated combinations exist in",sheetname,"of translation workbook. Remove duplicates before proceeding."))}
}
	
#Remove excess columns (if they exist, could feed additional columns to remove from join at this step)
screen_table=screen_table[,!names(screen_table) %in% c("InData","DateAdded")]

#Rename IR_FLAG & IR_COMMENT columns to flag_col_name & com_col_name
names(screen_table)[names(screen_table)=="IR_FLAG"]=flag_col_name
names(screen_table)[names(screen_table)=="IR_COMMENT"]=com_col_name

#Set blanks in data to NA for merge
data[data==""]=NA 

#Check that dataset does not contain flag column from domain table (indicates re-screening data, which leads to errors)

if(flag_col_name%in%colnames(data)){
  print("WARNING: Shared flag columns between result data and domain table may indicate that the screen table was already applied. If you have made changes to the domain table(s) between screening functions, either re-run with an unscreened dataset (post-fillMaskedValues object) or provide new flag_col_name and com_col_name before proceeding.")
  readline(prompt="Press [enter] to continue or [esc] to exit the function.")
  }

#Merge data and screen_table
data_screen=merge(data, screen_table, all.x=T)

#Check that merged_result data frame does not contain combinations not present in domain tables
if(any(is.na(data_screen[,names(data_screen)%in%c(flag_col_name)]))){
  warning(paste0("NA's generated in ",flag_col_name,". This can occur for two reasons: (1) Combinations exist in merged_results that do not exist in ",sheetname,", or (2) if na_dup_err=FALSE, NA's are likely present in domain table IR_FLAG. After making changes in domain table, re-run all applyScreenTables on a post-fillMaskedValues object to ensure changes are correctly merged."))
}

#Check that deimension[1] has stayed consistent
if(dim(data_screen)[1]!=dim(data)[1]){
	stop("Error: dimension[1] of input != dimension[1] of output. This is usually a startRow input arg error...")
}

return(data_screen)


}






