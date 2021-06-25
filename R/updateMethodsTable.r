#' Update paramTransTable, paramFractionGroup, & WQPParamCASID tables in translation workbook
#'
#' This function identifies parameter/fraction/unit/method combinations in WQP data for which an IR translation has not yet been defined and appends them to the translation table for review.
#'
#' @param data A WQP results (merged or narrow result) R-object
#' @param translation_wb Full path and filename for IR translation workbook
#' @param methodsTransTable_sheetname Name of sheet in workbook holding parameter translation table. Defaults to "paramTransTable".

#' @return Appends any new parameter/fraction/unit combinations in WQP data to translation_wb. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames

#' @export
updateMethodTrans=function(data, translation_wb, methodsTransTable_sheetname="paramMethodsTable"){

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

# Open methodsTransTable
methodtrans = unique(data.frame(openxlsx::readWorkbook(trans_wb, sheet = methodsTransTable_sheetname)))
methodtrans$DateAdded = openxlsx::convertToDate(methodtrans$DateAdded)
methodtrans = methodtrans[,!names(methodtrans)%in%c("InData")]

# Obtain methods info from data
dat_methods = unique(data[,c("ActivityMediaName","CharacteristicName","CAS","MethodSpecificationName","ResultSampleFractionText","ResultMeasure.MeasureUnitCode","ResultAnalyticalMethod.MethodIdentifier","ResultAnalyticalMethod.MethodIdentifierContext","ResultAnalyticalMethod.MethodName","ResultAnalyticalMethod.MethodQualifierTypeName","MethodDescriptionText")])
dat_methods$InData = "Y"

#Merge parameters w/ translation table (all=T (full outer join) gives a new translation table with all rows in translation table + any new parameters)
methods_new = merge(methodtrans, dat_methods, all = TRUE)

new_methods_count=dim(methods_new)[1]-dim(methodtrans)[1]
methods_new$DateAdded[is.na(methods_new$DateAdded)]=Sys.Date()

openxlsx::writeData(trans_wb, methodsTransTable_sheetname, methods_new)
print(paste("methodTransTable updated.", new_methods_count,"new method combinations identified."))

openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")


}