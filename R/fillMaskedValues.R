#' Select quantitation limits, determine detection condition, and fill masked values as appropriate.
#' 
#'This function selects a single upper and lower quantitation limit for each record in result (if one exists in detquantlim), determines detection condition for each record based on the presence/absence of a result value, upper or lower quant limits, and whether the result value is above or below quant limits or <=0.
#' 

#' @param results A WQP results (must include narrow result, merged, wide objects OK) R-object name.
#' @param detquantlim A WQP detection/quantitation limit file R-object. Should be matching complement to WQP results input.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx). 

#' @param sheetname  Name of sheet in workbook holding detection limit type names and ranked prioritizations table. Defaults to "detLimitTypeTable".
#' @param startRow Row to start reading the detLimitTypeTable excel sheet from (in case headers have been added). Defaults to 3.
#' @param lql_fac Numeric - factor by which to multiply lower quantitation limit type values when filling masked data or other non-detects (e.g. below lql values). Default = 0.5.
#' @param uql_fac Numeric - factor by which to multiply upper quantitation limit type values when filling masked data or other over limit values. Default = 1.

#' @return Returns a data frame with new columns of selected limits, filled values, units, and detection condition appended to results input. In IR_DetCond column, "ND"=non-detect, "OD"=over detection, "NRV"=no result value & no ranked detection limit, "DET"=detection.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames


#' @export
fillMaskedValues = function(results, detquantlim, translation_wb, sheetname="detLimitTypeTable", startRow=3, lql_fac=0.5, uql_fac=1){


####TESTING SETUP
####
#
#results=merged_results
#detquantlim=detquantlim
#translation_wb="P:\\WQ\\Integrated Report\\IR_2020\\2020-IR-assessments\\translations\\ir_translation_workbook.xlsx"
#sheetname="detLimitTypeTable"
#lql_fac=0.5
#uql_fac=1
#startRow=3
#######
#######


###Selecting upper and lower limit types by translation wb rankings

#Load translation workbook
trans_wb=loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
	removeFilter(trans_wb, sheetnames[n])
	}


#Join rankings from detLimitTypeTable to dql
detLimitTypeTable=data.frame(readWorkbook(trans_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE))
detLimitTypeTable=detLimitTypeTable[,c("DetectionQuantitationLimitTypeName","IRLimitPriorityRanking_lower","IRLimitPriorityRanking_upper")]

dql=merge(detquantlim,detLimitTypeTable,all.x=T)
dim(dql)
head(dql)
length(unique(dql$ResultIdentifier))


#Remove limits w/ 0s, NAs, ""s, etc in values and NAs in units from dql
dql$DetectionQuantitationLimitMeasure.MeasureValue[dql$DetectionQuantitationLimitMeasure.MeasureValue==""]=NA
dql$DetectionQuantitationLimitMeasure.MeasureValue[dql$DetectionQuantitationLimitMeasure.MeasureValue==0]=NA
dql$DetectionQuantitationLimitMeasure.MeasureUnitCode[dql$DetectionQuantitationLimitMeasure.MeasureUnitCode==""]=NA
dql$DetectionQuantitationLimitMeasure.MeasureUnitCode[dql$DetectionQuantitationLimitMeasure.MeasureUnitCode==0]=NA
dql=dql[!is.na(dql$DetectionQuantitationLimitMeasure.MeasureValue) & !is.na(dql$DetectionQuantitationLimitMeasure.MeasureUnitCode),]


#Set rankings for unranked to 99999
dql[is.na(dql$IRLimitPriorityRanking_lower),"IRLimitPriorityRanking_lower"]=99999
dql[is.na(dql$IRLimitPriorityRanking_upper),"IRLimitPriorityRanking_upper"]=99999

#Split to upper and lower
dql_up=dql[,!names(dql) %in% "IRLimitPriorityRanking_lower"]
dql_lo=dql[,!names(dql) %in% "IRLimitPriorityRanking_upper"]
names(dql_up)[names(dql_up)=="IRLimitPriorityRanking_upper"]="rank"
names(dql_lo)[names(dql_lo)=="IRLimitPriorityRanking_lower"]="rank"
dim(dql_up)
dim(dql_lo)

head(table(dql_up$ResultIdentifier)[table(dql_up$ResultIdentifier)>=3])

#Select one lim for each using min available rank (see https://stackoverflow.com/questions/15698749/select-one-row-in-a-group-based-on-the-first-appearance-of-a-value)
selectLim <- function(x) replace(logical(length(x)), which.min(x), TRUE)
dql_lo=transform(dql_lo, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_lo)
dql_lo[dql_lo$ResultIdentifier=="STORET-550706589",]
if(length(unique(dql_lo$ResultIdentifier))!=dim(dql_lo[dql_lo$keep==TRUE,])[1]){stop("Error: selected limits not unique to each RID...")}
dql_lo=dql_lo[dql_lo$keep==TRUE,!names(dql_lo) %in% "keep"]
head(dql_lo)
dql_lo[dql_lo$ResultIdentifier=="STORET-550706589",]

dql_up=transform(dql_up, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_up)
dql_up[dql_up$ResultIdentifier=="STORET-550706589",]
if(length(unique(dql_up$ResultIdentifier))!=dim(dql_up[dql_up$keep==TRUE,])[1]){stop("Error: selected limits not unique to each RID...")}
dql_up=dql_up[dql_up$keep==TRUE,!names(dql_up) %in% "keep"]
head(dql_up)
dql_up[dql_up$ResultIdentifier=="STORET-550706589",]


#Rename then combine to "wide" limit object w/ upper and lower limits
names(dql_lo)[names(dql_lo)=="DetectionQuantitationLimitTypeName"]="IR_LowerLimitType"
names(dql_lo)[names(dql_lo)=="rank"]="IR_LowerLimitRank"
names(dql_lo)[names(dql_lo)=="DetectionQuantitationLimitMeasure.MeasureValue"]="IR_LowerLimitValue"
names(dql_lo)[names(dql_lo)=="DetectionQuantitationLimitMeasure.MeasureUnitCode"]="IR_LowerLimitUnit"

names(dql_up)[names(dql_up)=="DetectionQuantitationLimitTypeName"]="IR_UpperLimitType"
names(dql_up)[names(dql_up)=="rank"]="IR_UpperLimitRank"
names(dql_up)[names(dql_up)=="DetectionQuantitationLimitMeasure.MeasureValue"]="IR_UpperLimitValue"
names(dql_up)[names(dql_up)=="DetectionQuantitationLimitMeasure.MeasureUnitCode"]="IR_UpperLimitUnit"

sel_dql=merge(dql_lo, dql_up)

if(length(unique(dql$ResultIdentifier))!=dim(sel_dql)[1]){stop("Error: selected limits not unique to each RID...")}

#Convert unranked (99999) lims back to NA
sel_dql[sel_dql$IR_LowerLimitRank==99999,c("IR_LowerLimitType","IR_LowerLimitValue","IR_LowerLimitUnit","IR_LowerLimitRank")]=NA
sel_dql[sel_dql$IR_UpperLimitRank==99999,c("IR_UpperLimitType","IR_UpperLimitValue","IR_UpperLimitUnit","IR_UpperLimitRank")]=NA


#Merge limits to results
results_dql=merge(results, sel_dql, all.x=T)

head(results_dql)
table(results_dql$IR_LowerLimitType)
table(results_dql$IR_UpperLimitType)


#######
#Filling values	
results_dql$ResultMeasureValue[results_dql$ResultMeasureValue==""]=NA #Convert blanks result values to NA

#Coercing values to numeric (need to double check w/ Emilie re: special characters in these columns)
suppressWarnings({
	results_dql$ResultMeasureValue=as.numeric(levels(results_dql$ResultMeasureValue))[results_dql$ResultMeasureValue]
	results_dql$IR_LowerLimitValue=as.numeric(levels(results_dql$IR_LowerLimitValue))[results_dql$IR_LowerLimitValue]
	results_dql$IR_UpperLimitValue=as.numeric(levels(results_dql$IR_UpperLimitValue))[results_dql$IR_UpperLimitValue]
	})


#Generate columns to be filled (fill w/ NA up front)
results_dql[,c("IR_Value","IR_Unit","IR_DetCond")]=NA
table(results_dql$IR_DetCond)			


###Determine detection conditions:

#is.na(rv) & !is.na(lql) ->ND
results_dql[is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_LowerLimitType)
			,"IR_DetCond"] = "ND"

table(results_dql$IR_DetCond)			

#is.na(rv) & is.na(lql) & !is.na(uql) ->OD
results_dql[is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_LowerLimitType)&
			!is.na(results_dql$IR_UpperLimitType)
			,"IR_DetCond"] = "OD"
table(results_dql$IR_DetCond)			

	
#is.na(rv) & is.na(lql) & is.na(uql) ->NRV
results_dql[is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_LowerLimitType)&
			is.na(results_dql$IR_UpperLimitType)
			,"IR_DetCond"] = "NRV"
table(results_dql$IR_DetCond)			

					
#!is.na(rv) & !is.na(uql) & is.na(lql) & rv>=uql ->OD
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>=results_dql$IR_UpperLimitValue
			,"IR_DetCond"] = "OD"
table(results_dql$IR_DetCond)			

#!is.na(rv) & !is.na(uql) & is.na(lql) rv<uql ->DET
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue<results_dql$IR_UpperLimitValue
			,"IR_DetCond"] = "DET"
table(results_dql$IR_DetCond)			

#!is.na(rv) & !is.na(uql) & !is.na(lql) & rv<=lql ->ND
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue<=results_dql$IR_LowerLimitValue
			,"IR_DetCond"] = "ND"
table(results_dql$IR_DetCond)			

#!is.na(rv) & !is.na(uql) & !is.na(lql) & rv>lql & rv>=uql ->OD
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>results_dql$IR_LowerLimitValue&
			results_dql$ResultMeasureValue>=results_dql$IR_UpperLimitValue
			,"IR_DetCond"] = "OD"
table(results_dql$IR_DetCond)			

#!is.na(rv) & !is.na(uql) & !is.na(lql) & rv>lql & rv<uql ->DET
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>results_dql$IR_LowerLimitValue&
			results_dql$ResultMeasureValue<results_dql$IR_UpperLimitValue
			,"IR_DetCond"] = "DET"
table(results_dql$IR_DetCond)			

#!is.na(rv) & is.na(uql) & !is.na(lql) & rv<=lql ->ND
results_dql[!is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue<=results_dql$IR_LowerLimitValue
			,"IR_DetCond"] = "ND"
table(results_dql$IR_DetCond)			

#!is.na(rv) & is.na(uql) & !is.na(lql) & rv>lql ->DET
results_dql[!is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>results_dql$IR_LowerLimitValue
			,"IR_DetCond"] = "DET"
table(results_dql$IR_DetCond)			

#!is.na(rv) & is.na(uql) & is.na(lql) & rv<=0 ->NRV
results_dql[!is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_UpperLimitType)&
			is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue<=0
			,"IR_DetCond"] = "NRV"
table(results_dql$IR_DetCond)			

#!is.na(rv) & is.na(uql) & is.na(lql) & rv>0 ->DET
results_dql[!is.na(results_dql$ResultMeasureValue)&
			is.na(results_dql$IR_UpperLimitType)&
			is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>0
			,"IR_DetCond"] = "DET"
table(results_dql$IR_DetCond)			

if(any(is.na(results_dql$IR_DetCond))){stop("ERROR - one or more detection conditions cannot be determined based on existing logic...")}


#Fill values & units based on IR_DetCond
results_dql[results_dql$IR_DetCond=="DET","IR_Value"]=results_dql[results_dql$IR_DetCond=="DET","ResultMeasureValue"]
results_dql[results_dql$IR_DetCond=="DET","IR_Unit"]=as.character(results_dql[results_dql$IR_DetCond=="DET","ResultMeasure.MeasureUnitCode"])

results_dql[results_dql$IR_DetCond=="ND","IR_Value"]=results_dql[results_dql$IR_DetCond=="ND","IR_LowerLimitValue"]*lql_fac
results_dql[results_dql$IR_DetCond=="ND","IR_Unit"]=as.character(results_dql[results_dql$IR_DetCond=="ND","IR_LowerLimitUnit"])

results_dql[results_dql$IR_DetCond=="OD","IR_Value"]=results_dql[results_dql$IR_DetCond=="OD","IR_UpperLimitValue"]*uql_fac
results_dql[results_dql$IR_DetCond=="OD","IR_Unit"]=as.character(results_dql[results_dql$IR_DetCond=="OD","IR_UpperLimitUnit"])

results_dql[results_dql$IR_DetCond=="NRV","IR_Value"]=NA
results_dql[results_dql$IR_DetCond=="NRV","IR_Unit"]=NA

return(results_dql)


}


