#' Update paramTransTable, paramFractionGroup, & WQPParamCASID tables in translation workbook
#'
#' This function identifies parameter/fraction/unit/method combinations in WQP data for which an IR translation has not yet been defined and appends them to the translation table for review.
#'
#' @param data A WQP results (merged or narrow result) R-object
#' @param detquantlim WQP detection/quantitation limit object
#' @param translation_wb Full path and filename for IR translation workbook

#' @param paramTransTable_sheetname Name of sheet in workbook holding parameter translation table. Defaults to "paramTransTable".
#' @param paramTransTable_startRow Row to start reading the paramTransTable excel sheet from (in case additional headers have been added). Defaults to 4.
#' @param paramTransTable_startCol Col to start writing the paramTransTable excel sheet to (to the right of all formula based columns). Defaults to 16.

#' @param WQPParamCASID_sheetname Name of sheet in workbook holding parameter/CAS ID table. Defaults to "WQPParamCASID".
#' @param WQPParamCASID_startRow Row to start reading the WQPParamCASID excel sheet from (in case additional headers have been added). Defaults to 3.
#' @param WQPParamCASID_startCol Col to start writing the WQPParamCASID excel sheet to (to the right of all formula based columns). Defaults to 1.

#' @param paramFractionGroup_sheetname Name of sheet in workbook holding the WQP fraction names. Defaults to "paramFractionGroup".
#' @param paramFractionGroup_startRow Row to start reading the paramFractionGroup excel sheet from (in case additional headers have been added). Defaults to 3.
#' @param paramFractionGroup_startCol Col to start writing the paramFractionGroup excel sheet to (to the right of all formula based columns, note col w/ header 'spacer' in table). Defaults to 3.

#' @return Appends any new parameter/fraction/unit combinations in WQP data to translation_wb. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames

#' @export
updateParamTrans=function(data, detquantlim=detquantlim, translation_wb,
						  paramTransTable_sheetname="paramTransTable", paramTransTable_startRow=4, paramTransTable_startCol=16,
						  WQPParamCASID_sheetname="WQPParamCASID", WQPParamCASID_startRow=4, WQPParamCASID_startCol=1,
						  paramFractionGroup_sheetname="paramFractionGroup", paramFractionGroup_startRow=3, paramFractionGroup_startCol=3
						){

###################						  
####TESTING SETUP
#translation_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\03translation\\IR_translation_workbook.xlsx"
#data=mrf_sub
#datatype="object"
#paramTransTable_sheetname="paramTransTable"
#paramTransTable_startRow=4
#paramTransTable_startCol=16
#WQPParamCASID_sheetname="WQPParamCASID"
#WQPParamCASID_startRow=4
#WQPParamCASID_startCol=1
#paramFractionGroup_sheetname="paramFractionGroup"
#paramFractionGroup_startRow=3
#paramFractionGroup_startCol=2
##fun_cols=c("CASLinked","R3172ParamAssessmentType","CASLinked","ParamAssessmentType","IR_ActivityType","AssessmentType","R3172ParameterStatus","R3172ParameterName",
##			"KeytoPrevIRParamTble","IR_Fraction","ToLabHoldingTime","ToExtractHoldingTime","AfterExtractHoldingTime",
##			"HoldingTimeFrozen","TotalHoldingTime","HoldingTimeUnit","Source")
#################


#Reading translation workbook and table
trans_wb=loadWorkbook(translation_wb)
sheetnames=getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	removeFilter(trans_wb, sheetnames[n])
	}

param_translation=data.frame(readWorkbook(trans_wb, sheet=paramTransTable_sheetname, startRow=paramTransTable_startRow, detectDates=TRUE))
param_translation=param_translation[,paramTransTable_startCol:dim(param_translation)[2]]

param_cas=data.frame(readWorkbook(trans_wb, sheet=WQPParamCASID_sheetname, startRow=WQPParamCASID_startRow, detectDates=TRUE))
param_cas=param_cas[,WQPParamCASID_startCol:dim(param_cas)[2]]


#Store input col names for future sorting
cnames=names(param_translation)
cnames_u=names(param_cas)

#Remove in current dataset column
param_translation=param_translation[,!names(param_translation)%in%"InData"]
param_cas=param_cas[,!names(param_cas)%in%"InData"]

#Merge detquantlim to data
data_dql=merge(data,detquantlim,all.x=T)

#Fill is.na units in data w/detquantlim units
data_dql$ResultMeasure.MeasureUnitCode[is.na(data_dql$ResultMeasure.MeasureUnitCode)]=data_dql$DetectionQuantitationLimitMeasure.MeasureUnitCode[is.na(data_dql$ResultMeasure.MeasureUnitCode)]

#Identifying unique parameters in input data
parameters=data.frame(unique(data_dql[,c("ActivityMediaName","CharacteristicName","MethodSpecificationName","ResultSampleFractionText","ResultMeasure.MeasureUnitCode","ResultAnalyticalMethod.MethodIdentifier","ResultAnalyticalMethod.MethodIdentifierContext","ResultAnalyticalMethod.MethodName","ResultAnalyticalMethod.MethodQualifierTypeName","MethodDescriptionText")]))
#Add in current dataset column to parameters before merging
parameters$InData="Y"
parameters[parameters==""]=NA #Set blanks to NA for merge

parameters_unique=data.frame(unique(data_dql[,"CharacteristicName"]))
names(parameters_unique)="DATACharacteristicName"
parameters_unique$InData="Y"
parameters_unique[parameters_unique==""]=NA #Set blanks to NA for merge

#Merge parameters w/ translation table (all=T (full outer join) gives a new translation table with all rows in translation table + any new parameters)
params_trans_merge=merge(parameters,param_translation,all=T)

parameters_unique_trans_merge=merge(parameters_unique,param_cas,all=T)

params_trans_merge=params_trans_merge[,cnames] #Reorder columns by translation table input
parameters_unique_trans_merge=parameters_unique_trans_merge[,cnames_u] #Reorder columns by translation table input


#Generate fractions table
fractions=data.frame(unique(data_dql$ResultSampleFractionText))
fraction_table=data.frame(readWorkbook(trans_wb, sheet=paramFractionGroup_sheetname, startRow=paramFractionGroup_startRow, detectDates=TRUE))
fraction_table=fraction_table[,paramFractionGroup_startCol:dim(fraction_table)[2]]
names(fractions)[1]=names(fraction_table)[1]
fractions_merge=merge(fractions,fraction_table,all=T)
new_fractions_count=dim(fractions_merge)[1]-dim(fraction_table)[1]


new_params_count=dim(params_trans_merge)[1]-dim(param_translation)[1]
new_params_cas_count=dim(parameters_unique_trans_merge)[1]-dim(param_cas)[1]
params_trans_merge$DateAdded[is.na(params_trans_merge$DateAdded)]=Sys.Date()

writeData(trans_wb, paramTransTable_sheetname, params_trans_merge, startRow=(paramTransTable_startRow+1), startCol=paramTransTable_startCol,colNames=F)
print(paste("paramTransTable updated.", new_params_count,"new parameter/fraction/unit/method combinations identified."))

writeData(trans_wb, paramFractionGroup_sheetname, fractions_merge, startRow=(paramFractionGroup_startRow+1), startCol=paramFractionGroup_startCol,colNames=F)
print(paste("paramFractionGroup table updated.", new_fractions_count,"new fractions identified."))

writeData(trans_wb, WQPParamCASID_sheetname, parameters_unique_trans_merge, startRow=(WQPParamCASID_startRow+1), startCol=WQPParamCASID_startCol,colNames=F)
print(paste("WQPParamCASID table updated.", new_params_cas_count,"new parameters identified."))


saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
print("Translation workbook updated & saved.")


}


















