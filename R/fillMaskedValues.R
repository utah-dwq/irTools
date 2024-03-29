#' Select quantitation limits, determine detection condition, and fill masked values as appropriate.
#'
#'This function selects a single upper and lower quantitation limit for each record in result (if one exists in detquantlim), determines detection condition for each record based on the presence/absence of a result value, upper or lower quant limits, and whether the result value is above or below quant limits or <=0.
#'

#' @param results A WQP results (must include narrow result, merged, wide objects OK) R-object name.
#' @param detquantlim A WQP detection/quantitation limit file R-object. Should be matching complement to WQP results input.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).

#' @param detLimitTypeTable_sheetname  Name of sheet in workbook holding detection limit type names and ranked prioritizations table. Defaults to "detLimitTypeTable".
#' @param unitConvTable_sheetname Name of sheet in workbook holding unit conversion table. Defaults to "unitConvTable".
#' @param detLimitTypeTable_startRow Row to start reading the detLimitTypeTable excel sheet from (in case headers have been added). Defaults to 2.
#' @param unitConvTable_startRow Row to start reading the unitConvTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param unitConvTable_startCol Column to start reading the unitConvTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param lql_fac Numeric - factor by which to multiply lower quantitation limit type values when filling masked data or other non-detects (e.g. below lql values). Default = 0.5.
#' @param uql_fac Numeric - factor by which to multiply upper quantitation limit type values when filling masked data or other over limit values. Default = 1.
#' @importFrom wqTools facToNum
#' @return Returns a data frame with new columns of selected limits, filled values, units, and detection condition appended to results input. In IR_DetCond column, "ND"=non-detect, "OD"=over detection, "NRV"=no result value & no ranked detection limit, "DET"=detection.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames
#' @importFrom dplyr rename
#' @importFrom wqTools facToNum

#' @export
fillMaskedValues = function(results, detquantlim, translation_wb, detLimitTypeTable_sheetname="detLimitTypeTable", unitConvTable_sheetname="unitConvTable", detLimitTypeTable_startRow=2, unitConvTable_startRow=1, unitConvTable_startCol=1, lql_fac=0.5, uql_fac=1){


#####TESTING SETUP
#####
# 
# results=merged_results
# detquantlim=detquantlim
# translation_wb="C:\\Users\\jvander\\Documents\\R\\irTools-test-16\\lookup-tables\\ir_translation_workbook.xlsx"
# detLimitTypeTable_sheetname="detLimitTypeTable"
# unitConvTable_sheetname="unitConvTable"
# lql_fac=0.5
# uql_fac=1
# detLimitTypeTable_startRow=2
# unitConvTable_startRow=1
# unitConvTable_startCol=1
########
########


###Selecting upper and lower limit types by translation wb rankings

#Load translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
	}


#Join rankings from detLimitTypeTable to dql
detLimitTypeTable=data.frame(openxlsx::readWorkbook(trans_wb, sheet=detLimitTypeTable_sheetname, startRow=detLimitTypeTable_startRow))
detLimitTypeTable=detLimitTypeTable[,c("DetectionQuantitationLimitTypeName","IRLimitPriorityRanking_lower","IRLimitPriorityRanking_upper")]

dql=merge(detquantlim,detLimitTypeTable,all.x=T)

# Dimension check following merge.
if(dim(detquantlim)[1]!=dim(dql)[1]){
  stop("Merge between detquantlim and detLimitTypeTable resulted in extra rows. Check for duplicates in detLimitTypeTable.")
}
dim(dql)
head(dql)
length(unique(dql$ResultIdentifier))


#Remove limits w/ 0s, NAs, ""s, etc in values and NAs in units from dql
# dql$DetectionQuantitationLimitMeasure.MeasureValue[dql$DetectionQuantitationLimitMeasure.MeasureValue==""]=NA
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

#Select one lim for each using min available rank (see https://stackoverflow.com/questions/15698749/select-one-row-in-a-group-based-on-the-first-appearance-of-a-value)
selectLim <- function(x) replace(logical(length(x)), which.min(x), TRUE)
dql_lo=transform(dql_lo, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_lo)
if(length(unique(dql_lo$ResultIdentifier))!=dim(dql_lo[dql_lo$keep==TRUE,])[1]){stop("Selected limits not unique to each RID...")}
dql_lo=dql_lo[dql_lo$keep==TRUE,!names(dql_lo) %in% "keep"]
head(dql_lo)

# dql_lo = dql_lo%>%group_by(ResultIdentifier)%>%slice_min(rank)%>%slice_head(n=1)
# test = names(table(dql_lo$ResultIdentifier)[table(dql_lo$ResultIdentifier)>1])
# if(length(test>0)){stop("ResultIdentifiers have multiple lower detection limits with the same ranking (likely 9999).")}

dql_up=transform(dql_up, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_up)
if(length(unique(dql_up$ResultIdentifier))!=dim(dql_up[dql_up$keep==TRUE,])[1]){stop("Selected limits not unique to each RID...")}
dql_up=dql_up[dql_up$keep==TRUE,!names(dql_up) %in% "keep"]
head(dql_up)

# dql_up = dql_up%>%group_by(ResultIdentifier)%>%slice_min(rank)%>%slice_head(n=1)
# test = names(table(dql_up$ResultIdentifier)[table(dql_up$ResultIdentifier)>1])
# if(length(test>0)){stop("ResultIdentifiers have multiple upper detection limits with the same ranking (likely 9999).")}
# 

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

#Check to ensure merge does not result in orphans.
if(length(unique(dql$ResultIdentifier))!=dim(sel_dql)[1]){stop("Selected limits not unique to each RID...")}

#Convert unranked (99999) lims back to NA
sel_dql[sel_dql$IR_LowerLimitRank==99999,c("IR_LowerLimitType","IR_LowerLimitValue","IR_LowerLimitUnit","IR_LowerLimitRank")]=NA
sel_dql[sel_dql$IR_UpperLimitRank==99999,c("IR_UpperLimitType","IR_UpperLimitValue","IR_UpperLimitUnit","IR_UpperLimitRank")]=NA


#Merge limits to results
results_dql=merge(results, sel_dql, all.x=T)

#######
#Filling values
results_dql$ResultMeasureValue[results_dql$ResultMeasureValue==""]=NA #Convert blanks result values to NA
results_dql$ResultMeasure.MeasureUnitCode[results_dql$ResultMeasure.MeasureUnitCode==""]=NA
results_dql$IR_UpperLimitUnit[results_dql$IR_UpperLimitUnit==""]=NA
results_dql$IR_LowerLimitUnit[results_dql$IR_LowerLimitUnit==""]=NA

#Coercing values to numeric if not already numeric (need to double check w/ Emilie re: special characters in these columns)
results_dql$ResultMeasureValue=wqTools::facToNum(results_dql$ResultMeasureValue)
results_dql$IR_LowerLimitValue=wqTools::facToNum(results_dql$IR_LowerLimitValue)
results_dql$IR_UpperLimitValue=wqTools::facToNum(results_dql$IR_UpperLimitValue)

###Unit checks between result values and limit values###
print("Checking for disparities in result units and limit units...")
##Pull out unique pairs of units##
#Result unit : lower limit unit
r_l_units <- data.frame(unique(results_dql[,c("IR_LowerLimitUnit", "ResultMeasure.MeasureUnitCode")]))
names(r_l_units)<- c("IR_Unit","CriterionUnits")
#Result unit :upper limit unit
r_u_units <- data.frame(unique(results_dql[,c("IR_UpperLimitUnit", "ResultMeasure.MeasureUnitCode")]))
names(r_u_units)<- c("IR_Unit","CriterionUnits")

#Merge unique unit combos together, remove lines with NA's, and retain only rows in which IR_Unit and CriterionUnits are different and need conversion.
r_lu_units <- merge(r_l_units,r_u_units, all=TRUE)
r_lu_units <- na.omit(r_lu_units)
r_lu_units$IR_Unit=as.character(r_lu_units$IR_Unit)
r_lu_units$CriterionUnits=as.character(r_lu_units$CriterionUnits)
r_lu_units <- r_lu_units[!r_lu_units$IR_Unit==r_lu_units$CriterionUnits,]

if(dim(r_lu_units)[1]>0){
  print("Unit conversion(s) needed between detection limit unit(s) and result unit(s). Checking for new unit conversions...")
  r_lu_units$InData = "Y"
  ##Merge to unitConvTable##
  unitconv_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet=unitConvTable_sheetname, startRow=unitConvTable_startRow))
  unitconv_table$DateAdded = openxlsx::convertToDate(unitconv_table$DateAdded)
  unitconv_table=unitconv_table[,!names(unitconv_table)%in%"InData"] # Refresh InData column 
  unitmerge <- merge(r_lu_units,unitconv_table, all=TRUE)
  
  ##Close out of function if new unit conversions added##
  if(!dim(unitmerge)[1]==dim(unitconv_table)[1]){
    unitmerge$DateAdded[is.na(unitmerge$DateAdded)]=Sys.Date() # this does not work with an empty dataframe
    openxlsx::writeData(trans_wb, "unitConvTable", unitmerge, startRow=unitConvTable_startRow, startCol=unitConvTable_startCol)
    #Save translation workbook
    openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
    newunit <- dim(unitmerge)[1]-dim(unitconv_table)[1]
    stop(paste(newunit,"new unit combination(s) detected. Populate conversion factor(s) in unit conversion table in translation workbook before re-running function."))
  }else{print("No new unit combinations detected. Proceeding to unit conversion...")}
  
  
  ##Attach unit conversion factors##
  unitconv <- subset(unitmerge, InData=="Y")
  unitconv <- unitconv[,names(unitconv)%in%c("IR_Unit","CriterionUnits","UnitConversionFactor")]
  
  #Check that UnitConversionFactor if filled in for all in unitconv
  if(any(is.na(unitconv$UnitConversionFactor))){stop("Needed unit conversion factor is NA in unit conversion table.")}
  
  #Rename columns to merge CvFs with results - upper
  unitconv=dplyr::rename(unitconv, IR_UpperLimitUnit=IR_Unit, ResultMeasure.MeasureUnitCode=CriterionUnits, IR_Unit_CvF_Upper=UnitConversionFactor)
  
  #Merge upper conversion factors to results
  dimcheck <- dim(results_dql)
  results_dql <- merge(results_dql,unitconv, all.x=TRUE)
  if(!dimcheck[1]==dim(results_dql)[1]){
    stop("Merge between result data and unit conversion table resulted in new rows. Check conversion table for missing or erroneous values.")
  }
  #Rename columns to merge CvFs with results - lower
  unitconv=dplyr::rename(unitconv, IR_LowerLimitUnit=IR_UpperLimitUnit, IR_Unit_CvF_Lower=IR_Unit_CvF_Upper)

  #Merge lower conversion factors to results
  dimcheck <- dim(results_dql)
  results_dql <- merge(results_dql,unitconv, all.x=TRUE)
  if(!dimcheck[1]==dim(results_dql)[1]){
    stop("Merge between result data and unit conversion table resulted in new rows. Check conversion table for missing or erroneous values.")
  }
  ##Make unit conversions##
  #Make conversions between result and limit units IFF unit conversion factor is not NA. 
  #Update limit units for limit values requiring a conversion factor to match result value/units.
head(results_dql[!is.na(results_dql$IR_Unit_CvF_Lower),])
  results_dql=within(results_dql,{
    IR_UpperLimitValue[!is.na(IR_Unit_CvF_Upper)]=IR_UpperLimitValue[!is.na(IR_Unit_CvF_Upper)]*IR_Unit_CvF_Upper[!is.na(IR_Unit_CvF_Upper)]
    IR_LowerLimitValue[!is.na(IR_Unit_CvF_Lower)]=IR_LowerLimitValue[!is.na(IR_Unit_CvF_Lower)]*IR_Unit_CvF_Lower[!is.na(IR_Unit_CvF_Lower)]
    IR_UpperLimitUnit[!is.na(IR_Unit_CvF_Upper)]=ResultMeasure.MeasureUnitCode[!is.na(IR_Unit_CvF_Upper)]
    IR_LowerLimitUnit[!is.na(IR_Unit_CvF_Lower)]=ResultMeasure.MeasureUnitCode[!is.na(IR_Unit_CvF_Lower)]
  })
head(results_dql[!is.na(results_dql$IR_Unit_CvF_Lower),])
  
  ##Remove conversion columns##
  results_dql <- results_dql[,!names(results_dql)%in%c("IR_Unit_CvF_Upper","IR_Unit_CvF_Lower")]
  
}else{print("No unit conversions needed. Proceeding to detection condition assignment...")}


#Generate columns to be filled (fill w/ NA up front)
results_dql[,c("IR_Value","IR_Unit","IR_DetCond")]=NA
table(results_dql$IR_DetCond)


###Determine detection conditions:

#is.na(rv) & !is.na(lql) ->ND
results_dql[is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_LowerLimitType)
			,"IR_DetCond"] = "ND"

table(results_dql$IR_DetCond)

#is.na(rv) & !is.na(lql) & !is.na(uql) - THIS IS A "FYI" CHECK

twolim <- length(results_dql[is.na(results_dql$ResultMeasureValue)&
            !is.na(results_dql$IR_LowerLimitType)&
            !is.na(results_dql$IR_UpperLimitType),1])


#### Consider incorporating ResultDetectionConditionText for this decision.
if(twolim>0){
  warning(paste("There are",twolim,"records with both upper and lower quantitation limits and is.na(result values). These records have been assigned as 'ND's"))
}

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

#!is.na(rv) & !is.na(uql) & rv>=uql ->OD (note that this includes records with !is.na(lql) and is.na(lql))
results_dql[!is.na(results_dql$ResultMeasureValue)&
              !is.na(results_dql$IR_UpperLimitType)&
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

#!is.na(rv) & !is.na(lql) & rv<=lql ->ND (note that this includes records with !is.na(uql) and is.na(uql))
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue<=results_dql$IR_LowerLimitValue
			,"IR_DetCond"] = "ND"
table(results_dql$IR_DetCond)

#!is.na(rv) & !is.na(uql) & !is.na(lql) & rv>lql & rv<uql ->DET
results_dql[!is.na(results_dql$ResultMeasureValue)&
			!is.na(results_dql$IR_UpperLimitType)&
			!is.na(results_dql$IR_LowerLimitType)&
			results_dql$ResultMeasureValue>results_dql$IR_LowerLimitValue&
			results_dql$ResultMeasureValue<results_dql$IR_UpperLimitValue
			,"IR_DetCond"] = "DET"
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

# Allow zero & negative values in profile depth measures, temperature, & flow
results_dql[which(results_dql$ResultMeasureValue<=0 & 
	(results_dql$CharacteristicName %in% c("Depth, data-logger (ported)","Temperature, water","Stream flow, instantaneous","Flow","Flow rate, instantaneous","Stream flow, mean. daily","Velocity-discharge","Escherichia coli")) & 
	!is.na(results_dql$ResultMeasureValue)),"IR_DetCond"] = "DET"
	
table(results_dql$IR_DetCond)

print("Detection condition counts:")
print(table(results_dql$IR_DetCond, exclude=NULL))

if(any(is.na(results_dql$IR_DetCond))){stop("One or more detection conditions cannot be determined based on existing logic. NA's present in IR_DetCond.")}

#check1 <- unique(results_dql[results_dql$IR_DetCond=="DET",c("ResultMeasure.MeasureUnitCode","IR_LowerLimitUnit")])

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
