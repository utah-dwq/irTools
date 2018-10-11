#' Assign numeric criteria to WQP data
#'
#' Assigns general & site-specific numeric WQ criteria to WQP data. Does not calculate criteria w/ correction factors. See calcCriteria (function in development).
#' @param data A merged, parameter translated, spatially referenced WQP result object.
#' @param crit_wb Full path and filename for workbook containing criteria.
#' @param crit_sheetname Name of sheet in workbook holding criteria to be assigned.
#' @param crit_startRow Row to start reading criteria table excel sheet from (in case headers have been added). Defaults to 1.
#' @param ss_sheetname Name of sheet in workbook holding site-specific criteria to be assigned. Should typically contain parameters corresponding to those in criteria table.
#' @param ss_startRow Row to start reading site specific criteria table excel sheet from (in case headers have been added). Defaults to 1.
#' @return A flattened & expanded sample x use x parameter x criterion data.frame.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom reshape2 colsplit
#' @importFrom plyr rbind.fill
#' @importFrom lubridate month
#' @export
assignCriteria=function(data, translation_wb, crit_sheetname, ss_sheetname, crit_startRow=1, ss_startRow=1){ 


###Assign numeric criteria to WQP data

###Inputs:
#1. Merged, translated, & spatially validated data (i.e. narrow result + activity file w/ all screen tables including sites applied)
#2. Criterion file (use x parameter)

####Rough outline:
#1. Flatten data by use
#	-Expand ben use col (BEN_CLASS = BUs assigned spatially as comma separated vector in single column)
#	-Flatten
#2. Read in criteria tables
#3. Merge criterion table to data (by BU + R3172ParameterName, all.x=T)
#4. Site specific standards
#	-ID records w/ SS criteria
#	-Remove those records from data_uses_flat_crit
#	-Merge SS criteria to data_uses_flat (only keep matches)
#	-Remove matches where MLID != SS MLID (if provided)
#	-rbind matches back to data_uses_flat_crit



####Testing setup
#library(openxlsx)
#library(reshape2)
#library(plyr)
#data=mrf_sub
#crit_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\04standards\\IR_uses_standards.xlsx"
#crit_sheetname="R317214DomesticRecAgCriteria_JV"
#ss_sheetname="R317214SSCriteria_JV"
#crit_startRow=1
#ss_startRow=1


#Load workbook
crit_wb=openxlsx::loadWorkbook(crit_wb)

#Read selected sheets in workbook table - note, these tables should be simplified/harmonized to just needed columns & eliminate overlap w/ parameter translation table (consider putting assessment & activity types here instead of param translation)
crit_table=data.frame(openxlsx::readWorkbook(crit_wb, sheet=crit_sheetname, startRow=crit_startRow, detectDates=TRUE))
ss_table=data.frame(openxlsx::readWorkbook(crit_wb, sheet=ss_sheetname, startRow=ss_startRow, detectDates=TRUE))

#Identify correction factor (CF) parameters in crit_table, append ",CF" to BEN_CLASS for all of these parameters (note, this is so target units can be defined for correction factors)
cf=crit_table[crit_table$BeneficialUse=="CF",]
data$BEN_CLASS[data$R3172ParameterName %in% cf$R3172ParameterName]=paste0(data$BEN_CLASS[data$R3172ParameterName %in% cf$R3172ParameterName],",CF")
table(is.na(data$BEN_CLASS))

#Expand data uses (BEN_CLASS)
max_use_count=max(sapply(strsplit(data$BEN_CLASS,","),FUN="length"))
use_colnames=paste0(rep("use",max_use_count),seq(1:max_use_count))
uses_mat=data.frame(data$BEN_CLASS,colsplit(data$BEN_CLASS,",",use_colnames))
names(uses_mat)[names(uses_mat)=="data.BEN_CLASS"]="BEN_CLASS"

#Flatten uses
uses_flat=melt(uses_mat, id.vars="BEN_CLASS", value.name = "BeneficialUse")
uses_flat=unique(uses_flat[,!names(uses_flat)=="variable"])
uses_flat=uses_flat[uses_flat$BeneficialUse!="" & !is.na(uses_flat$BeneficialUse),]

#Merge flat uses back to data (by BEN_CLASS)
data_uses_flat=merge(data,uses_flat,all=T)

#Merge criteria to data w/ flattened uses
data_uses_flat_crit=merge(data_uses_flat,crit_table,all.x=T)
dim_check=dim(data_uses_flat_crit)[1]

head(data_uses_flat_crit[which(data_uses_flat_crit$R3172ParameterName=="Magnesium" & is.na(data_uses_flat_crit$NumericCriteria) & data_uses_flat_crit$BeneficialUse=="CF"),])


#4. Site specific standards
#ID records w/ SS criteria (by SSCLocDescription)
#Remove those records from data_uses_flat_crit
ssc=unique(ss_table[,c("BeneficialUse","ss_R317Descrp","R3172ParameterName")])
ssc$ssc="Y"
data_uses_flat_crit=merge(data_uses_flat_crit, ssc, all.x=T)
	dim(data_uses_flat_crit)[1]==dim_check
	table(is.na(data_uses_flat_crit$ssc))
data_uses_flat_crit=data_uses_flat_crit[is.na(data_uses_flat_crit$ssc),]
	dim(data_uses_flat_crit)[1]<dim_check
	table(is.na(data_uses_flat_crit$ssc))
data_uses_flat_crit=data_uses_flat_crit[,!names(data_uses_flat_crit) %in% "ssc"]

#Merge SS criteria to data_uses_flat (by parameter and ss location description, only keep matches) in new object
data_uses_flat_ssc=merge(data_uses_flat,ss_table)
head(data_uses_flat_ssc)
dim(data_uses_flat_ssc)

#Remove matches where MLID != SS MLID (if provided)
dim(data_uses_flat_ssc)
data_uses_flat_ssc=data_uses_flat_ssc[data_uses_flat_ssc$MonitoringLocationIdentifier==data_uses_flat_ssc$SSC_MLID | is.na(data_uses_flat_ssc$SSC_MLID),]
dim(data_uses_flat_ssc)

#remove records where month is outside SS months (if provided)
data_uses_flat_ssc$month=lubridate::month(data_uses_flat_ssc$ActivityStartDate)

data_uses_flat_ssc$in_ssc_period=
	((data_uses_flat_ssc$month>=data_uses_flat_ssc$SSC_StartMon & data_uses_flat_ssc$month<=data_uses_flat_ssc$SSC_EndMon) | 
	(data_uses_flat_ssc$SSC_StartMon > data_uses_flat_ssc$SSC_EndMon & (data_uses_flat_ssc$month<=data_uses_flat_ssc$SSC_EndMon | data_uses_flat_ssc$month>=data_uses_flat_ssc$SSC_StartMon)))

table(data_uses_flat_ssc$in_ssc_period)

###Double checking the between start and end month logic...
#data.frame(
#	data_uses_flat_ssc$month,
#	data_uses_flat_ssc$SSC_StartMon,
#	data_uses_flat_ssc$SSC_EndMon,
#	((data_uses_flat_ssc$month>=data_uses_flat_ssc$SSC_StartMon & data_uses_flat_ssc$month<=data_uses_flat_ssc$SSC_EndMon) | 
#	(data_uses_flat_ssc$SSC_StartMon > data_uses_flat_ssc$SSC_EndMon & (data_uses_flat_ssc$month<=data_uses_flat_ssc$SSC_EndMon | data_uses_flat_ssc$month>=data_uses_flat_ssc$SSC_StartMon)))
#)


dim(data_uses_flat_ssc)
data_uses_flat_ssc=data_uses_flat_ssc[data_uses_flat_ssc$in_ssc_period=="TRUE" | is.na(data_uses_flat_ssc$in_ssc_period),]
data_uses_flat_ssc=data_uses_flat_ssc[,!names(data_uses_flat_ssc) %in% c("in_ssc_period","month")]
dim(data_uses_flat_ssc)


#rbind (w/ fill) matches back to data_uses_flat_crit
dim(data_uses_flat_crit)
data_uses_flat_crit=plyr::rbind.fill(data_uses_flat_crit, data_uses_flat_ssc)
dim(data_uses_flat_crit)


#5. Calculated criteria
#JV note I think this will be better done separately (prob stand-alone function), following unit conversions.

#Correction factors
#pH, temp, hardness (Ca+Mg > hardness >100(?), max=400 mg/l)

#-Grab records that need criteria calculated (remove these from data?)
#-Build correction factor subdataset
#-Cast correction factor subdataset
#-Merge correction factors to records needing calculated criteria
#-Apply formulas to calculate criteria
#-rbind corrections back to data (keep columns of correction factors, fill w/ NA for other data)


return(data_uses_flat_crit)


}
