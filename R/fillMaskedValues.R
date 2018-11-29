#' Select quantitation limits, determine detection condition, and fill masked values as appropriate.
#'
#'This function selects a single upper and lower quantitation limit for each record in result (if one exists in detquantlim), determines detection condition for each record based on the presence/absence of a result value, upper or lower quant limits, and whether the result value is above or below quant limits or <=0.
#'

#' @param results A WQP results (must include narrow result, merged, wide objects OK) R-object name.
#' @param detquantlim A WQP detection/quantitation limit file R-object. Should be matching complement to WQP results input.
#' @param translation_wb Full path and filename for IR translation workbook (.xlsx).

#' @param detsheetname  Name of sheet in workbook holding detection limit type names and ranked prioritizations table. Defaults to "detLimitTypeTable".
#' @param unitsheetname Name of sheet in workbook holding unit conversion table. Defaults to "unitConvTable".
#' @param detstartRow Row to start reading the detLimitTypeTable excel sheet from (in case headers have been added). Defaults to 3.
#' @param unitstartRow Row to start reading the unitConvTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param unitstartCol Column to start reading the unitConvTable excel sheet from (in case headers have been added). Defaults to 1.
#' @param lql_fac Numeric - factor by which to multiply lower quantitation limit type values when filling masked data or other non-detects (e.g. below lql values). Default = 0.5.
#' @param uql_fac Numeric - factor by which to multiply upper quantitation limit type values when filling masked data or other over limit values. Default = 1.

#' @return Returns a data frame with new columns of selected limits, filled values, units, and detection condition appended to results input. In IR_DetCond column, "ND"=non-detect, "OD"=over detection, "NRV"=no result value & no ranked detection limit, "DET"=detection.

#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx removeFilter
#' @importFrom openxlsx getSheetNames


#' @export
fillMaskedValues = function(results, detquantlim, translation_wb, detsheetname="detLimitTypeTable", unitsheetname="unitConvTable", detstartRow=3, unitstartRow=1, unitstartCol=1, lql_fac=0.5, uql_fac=1){


####TESTING SETUP
####
# 
# results=merged_results
# detquantlim=detquantlim
# translation_wb="C:\\Users\\ehinman\\Documents\\GitHub\\lookup_tables\\ir_translation_workbook.xlsx"
# detsheetname="detLimitTypeTable"
# unitsheetname="unitConvTable"
# lql_fac=0.5
# uql_fac=1
# detstartRow=3
# unitstartRow=1
# unitstartCol=1
#######
#######


###Selecting upper and lower limit types by translation wb rankings

#Load translation workbook
trans_wb=openxlsx::loadWorkbook(translation_wb)

#Remove filters from all sheets in trans_wb (filtering seems to cause file corruption occassionally...)
sheetnames=openxlsx::getSheetNames(translation_wb)
for(n in 1:length(sheetnames)){
  openxlsx::removeFilter(trans_wb, sheetnames[n])
	}


#Join rankings from detLimitTypeTable to dql
detLimitTypeTable=data.frame(openxlsx::readWorkbook(trans_wb, sheet=detsheetname, startRow=detstartRow, detectDates=TRUE))
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

#Select one lim for each using min available rank (see https://stackoverflow.com/questions/15698749/select-one-row-in-a-group-based-on-the-first-appearance-of-a-value)
selectLim <- function(x) replace(logical(length(x)), which.min(x), TRUE)
dql_lo=transform(dql_lo, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_lo)
dql_lo[dql_lo$ResultIdentifier=="STORET-550706589",]
if(length(unique(dql_lo$ResultIdentifier))!=dim(dql_lo[dql_lo$keep==TRUE,])[1]){stop("Error: selected limits not unique to each RID...")}
dql_lo=dql_lo[dql_lo$keep==TRUE,!names(dql_lo) %in% "keep"]
head(dql_lo)

dql_up=transform(dql_up, keep=as.logical(ave(rank, ResultIdentifier, FUN=selectLim)))
dim(dql_up)
if(length(unique(dql_up$ResultIdentifier))!=dim(dql_up[dql_up$keep==TRUE,])[1]){stop("Error: selected limits not unique to each RID...")}
dql_up=dql_up[dql_up$keep==TRUE,!names(dql_up) %in% "keep"]
head(dql_up)


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
if(length(unique(dql$ResultIdentifier))!=dim(sel_dql)[1]){stop("Error: selected limits not unique to each RID...")}

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
suppressWarnings({
	if(class(results_dql$ResultMeasureValue)!="numeric"){
		results_dql$ResultMeasureValue=as.numeric(levels(results_dql$ResultMeasureValue))[results_dql$ResultMeasureValue]}
	if(class(results_dql$IR_LowerLimitValue)!="numeric"){
		results_dql$IR_LowerLimitValue=as.numeric(levels(results_dql$IR_LowerLimitValue))[results_dql$IR_LowerLimitValue]}
	if(class(results_dql$IR_UpperLimitValue)!="numeric"){
		results_dql$IR_UpperLimitValue=as.numeric(levels(results_dql$IR_UpperLimitValue))[results_dql$IR_UpperLimitValue]}
	})

###Unit checks between result values and limit values###
print("Checking for disparities in result units and limit units...")
##Pull out unique pairs of units##
#result unit : lower limit unit
r_l_units <- data.frame(unique(results_dql[,c("IR_LowerLimitUnit", "ResultMeasure.MeasureUnitCode")]))
names(r_l_units)<- c("IR_Unit","CriterionUnits")
#result unit :upper limit unit
r_u_units <- data.frame(unique(results_dql[,c("IR_UpperLimitUnit", "ResultMeasure.MeasureUnitCode")]))
names(r_u_units)<- c("IR_Unit","CriterionUnits")
#merge unique unit combos together and remove lines with NA's
r_lu_units <- merge(r_l_units,r_u_units, all=TRUE)
r_lu_units <- na.omit(r_lu_units)
r_lu_units$IR_Unit=as.character(r_lu_units$IR_Unit) # does as.factor or as.character matter?
r_lu_units$CriterionUnits=as.character(r_lu_units$CriterionUnits)
same <- mapply(identical,r_lu_units$IR_Unit,r_lu_units$CriterionUnits)
r_lu_units <- r_lu_units[!same,]
r_lu_units$InData = "Y"

##Merge to unitConvTable##
unitconv_table=data.frame(openxlsx::readWorkbook(trans_wb, sheet=unitsheetname, startRow=unitstartRow, detectDates=TRUE))
unitconv_table=unitconv_table[,!names(unitconv_table)%in%"InData"] # Refresh InData column 
unitmerge <- merge(r_lu_units,unitconv_table, all=TRUE)

##Close out of function if new unit conversions added##
if(!dim(unitmerge)[1]==dim(unitconv_table)[1]){
  unitmerge$DateAdded[is.na(unitmerge$DateAdded)]=Sys.Date() # this does not work with an empty dataframe
  openxlsx::writeData(trans_wb, "unitConvTable", unitmerge, startRow=unitstartRow, startCol=unitstartCol)
  #Save translation workbook
  openxlsx::saveWorkbook(trans_wb, translation_wb, overwrite = TRUE)
  newunit <- dim(unitmerge)[1]-dim(unitconv_table)[1]
  stop(paste(newunit,"new unit combination(s) detected. Populate conversion factor(s) in translation workbook before re-running function."))
}else{print("No new unit combinations detected. Proceeding to unit conversion...")}

##Attach unit conversion factors##
unitconv <- subset(unitmerge, unitmerge$InData=="Y")
unitconv <- unitconv[,names(unitconv)%in%c("IR_Unit","CriterionUnits","UnitConversionFactor")]
#Rename columns to merge CvFs with results - upper
names(unitconv)<- c("IR_UpperLimitUnit","ResultMeasure.MeasureUnitCode","IR_Unit_CvF_Upper")
#Merge upper conversion factors to results
dimcheck <- dim(results_dql)
results_dql <- merge(results_dql,unitconv, all.x=TRUE)
if(!dimcheck[1]==dim(results_dql)[1]){
  stop("Merge between result data and unit conversion table resulted in new rows. Check conversion table for missing or erroneous values.")
}
#Rename columns to merge CvFs with results - lower
names(unitconv)<- c("IR_LowerLimitUnit","ResultMeasure.MeasureUnitCode","IR_Unit_CvF_Lower")
#Merge lower conversion factors to results
dimcheck <- dim(results_dql)
results_dql <- merge(results_dql,unitconv, all.x=TRUE)
if(!dimcheck[1]==dim(results_dql)[1]){
  stop("Merge between result data and unit conversion table resulted in new rows. Check conversion table for missing or erroneous values.")
}
##Make unit conversions##
#Make conversions between result and limit units IFF unit conversion factor is not NA. 
results_dql=within(results_dql,{
  IR_UpperLimitValue[!is.na(IR_Unit_CvF_Upper)]=IR_UpperLimitValue[!is.na(IR_Unit_CvF_Upper)]*IR_Unit_CvF_Upper[!is.na(IR_Unit_CvF_Upper)]
  IR_LowerLimitValue[!is.na(IR_Unit_CvF_Lower)]=IR_LowerLimitValue[!is.na(IR_Unit_CvF_Lower)]*IR_Unit_CvF_Lower[!is.na(IR_Unit_CvF_Lower)]
  IR_UpperLimitUnit[!is.na(IR_Unit_CvF_Upper)]=ResultMeasure.MeasureUnitCode[!is.na(IR_Unit_CvF_Upper)]
  IR_LowerLimitUnit[!is.na(IR_Unit_CvF_Lower)]=ResultMeasure.MeasureUnitCode[!is.na(IR_Unit_CvF_Lower)]
  })

##Remove conversion columns##
results_dql <- results_dql[,!names(results_dql)%in%c("IR_Unit_CvF_Upper","IR_Unit_CvF_Lower")]

####Unit conversion fix outline (treating result units as target units, convert & fill limit units in place, maintain raw value and units in original columns):
#1. Set all values to NA where is.na(associated unit) - this will prevent us from comparing values where one or more unit is an NA
#2. Pull out all unique pairs of units:
	#a. result unit : lower limit unit
	#b. result unit : upper limit unit
	#c. rbind together & na.omit (as separate object to check if any additional conversions are needed)
	#d. If additional conversions are required, append to conversion table and update workbook, exit with message directing user to update the conversion table as appropriate
#3. For both sets of units:
	#a. merge conversion factor from unit conv table to result unit : limit unit (renaming columns as appropriate)
	#b. na.omit() to remove any records where one or both units are NA (this will result in NA conversion factor when merged)
	#c. merge result unit : limit unit object +conversion factor to data (one at a time or two cols w/ different names e.g lolim_conv_fact & uplim_conv_fact)
	#d. convert limit value & update units only where data[!is.na(data$conversion_factor)] - this will keep values & units that were already NA as NA
	#e. remove limit conversion factor(s)
#I think this will fit seamlessly with the rest of the function code...

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
if(twolim>0){
  warning(paste("FYI: There are",twolim,"records with both upper and lower quantitation limits and is.na(result values). These records have been assigned as 'ND's"))
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

print("Detection condition counts:")
print(table(results_dql$IR_DetCond, exclude=NULL))

if(any(is.na(results_dql$IR_DetCond))){stop("ERROR - one or more detection conditions cannot be determined based on existing logic. NA's present in IR_DetCond.")}

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
