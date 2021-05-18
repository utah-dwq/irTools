#' Update paramTransTable, paramFractionGroup, & WQPParamCASID tables in translation workbook
#'
#' This function identifies parameter/fraction/unit/method combinations in WQP data for which an IR translation has not yet been defined and appends them to the translation table for review.
#'
#' @param data A WQP results (merged or narrow result) R-object
#' @param translation_wb Full path and filename for IR translation workbook
#' @param standards_wb Full path and filename for IR standards workbook

#' @param paramTransTable_sheetname Name of sheet in workbook holding parameter translation table. Defaults to "paramTransTable".
#' @param paramTransTable_startRow Row to start reading the paramTransTable excel sheet from (in case additional headers have been added). Defaults to 1.
#' @param paramTransTable_startCol Col to start writing the paramTransTable excel sheet to (to the right of all formula based columns). Defaults to 1.
#' 
#' @param CASLookup_sheetname Name of sheet in workbook holding CAS lookup table from WQP. Defaults to "CASLookup".
#' @param CASLookup_startRow Row to start reading the CASLookup excel sheet from (in case additional headers have been added). Defaults to 2.
#' @param CASLookup_startCol Col to start writing the CASLookup excel sheet to (to the right of all formula based columns). Defaults to 1.

#' @param paramFractionGroup_sheetname Name of sheet in workbook holding the WQP fraction names. Defaults to "paramFractionGroup".
#' @param paramFractionGroup_startRow Row to start reading the paramFractionGroup excel sheet from (in case additional headers have been added). Defaults to 2.
#' @param paramFractionGroup_startCol Col to start writing the paramFractionGroup excel sheet to (to the right of all formula based columns, note col w/ header 'spacer' in table). Defaults to 2.

#' @return Appends any new parameter/fraction/unit combinations in WQP data to translation_wb. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames

#' @export
updateParamTrans=function(data, translation_wb, standards_wb,
						  paramTransTable_sheetname="paramTransTable", paramTransTable_startRow=1, paramTransTable_startCol=1,
						  CASLookup_sheetname="CASLookup", CASLookup_startRow=2, CASLookup_startCol=1,
						  paramFractionGroup_sheetname="paramFractionGroup", paramFractionGroup_startRow=2, paramFractionGroup_startCol=2
						){

###################						  
####TESTING SETUP
# translation_wb = "C:\\Users\\ehinman\\Desktop\\translation_workbook_2022_redo_draft.xlsx"

##################

# Read in translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
}

# Open CAS Lookup table from WQP
caslookup = data.frame(openxlsx::readWorkbook(trans_wb, sheet=CASLookup_sheetname, startRow=CASLookup_startRow, detectDates=TRUE))
names(caslookup)[names(caslookup)=="Name"] = "CharacteristicName"
names(caslookup)[names(caslookup)=="CAS.Number"] = "CAS_WQP"

# Match up CAS numbers to parameter names
chars = data.frame("CharacteristicName" = unique(update_table_data$CharacteristicName))
chars$InData = "Y"
chars_cas = merge(chars, caslookup, all.x = TRUE)
chars_cas1 = chars_cas[,names(chars_cas)%in%c("CharacteristicName","CAS_WQP","InData")]

# Open paramTransTable
paramtrans = data.frame(openxlsx::readWorkbook(trans_wb, sheet = "paramTransTable", startRow = paramTransTable_startRow))
paramtrans$DateAdded = as.Date(paramtrans$DateAdded, origin = "1899-12-30")
paramtrans = paramtrans[,!names(paramtrans)%in%c("InData","CAS_WQP","CAS_check","InStandards")]

#Merge parameters w/ translation table (all=T (full outer join) gives a new translation table with all rows in translation table + any new parameters)
param_new = merge(paramtrans, chars_cas1, all = TRUE)
param_new$CAS_check = ifelse(param_new$CAS_DWQ==param_new$CAS_WQP, "same","different")

# Open standards table and extract CAS numbers
crit= data.frame(openxlsx::readWorkbook(standards_wb,sheet = "criteria", startRow = 3))
crit = unique(crit$CAS)
ss_crit = data.frame(openxlsx::readWorkbook(standards_wb,sheet = "ss_criteria", startRow = 4))
ss_crit = unique(ss_crit$CAS)
crits = unique(c(crit, ss_crit))

# Flag data records with CAS numbers represented in standards
paramtrans_crit = param_new
paramtrans_crit$CAS_in_Stds = ifelse(paramtrans_crit$CAS_WQP%in%crits,"Y",NA)

#Generate fractions table by merging fractions from activity/narrowresult data to detquantlim data.
fractions=data.frame(unique(update_table_data$ResultSampleFractionText))
fraction_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet=paramFractionGroup_sheetname, startRow=paramFractionGroup_startRow, detectDates=TRUE))
fraction_table=fraction_table[,paramFractionGroup_startCol:dim(fraction_table)[2]]
names(fractions)[1]=names(fraction_table)[1]
fractions_merge=merge(fractions,fraction_table,all=T)
new_fractions_count=dim(fractions_merge)[1]-dim(fraction_table)[1]

new_params_count=dim(paramtrans_crit)[1]-dim(paramtrans)[1]
paramtrans_crit$DateAdded[is.na(paramtrans_crit$DateAdded)]=Sys.Date()

openxlsx::writeData(trans_wb, paramTransTable_sheetname, paramtrans_crit)
print(paste("paramTransTable updated.", new_params_count,"new parameter/CAS combinations identified."))

openxlsx::writeData(trans_wb, paramFractionGroup_sheetname, fractions_merge, startRow = paramFractionGroup_startRow, startCol = paramFractionGroup_startCol)
print(paste("paramFractionGroup table updated.", new_fractions_count,"new fractions identified."))

openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")


}


















