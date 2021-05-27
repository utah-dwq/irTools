#' Assign numeric criteria to WQP data
#'
#' Assigns general & site-specific numeric WQ criteria to WQP data. Does not calculate criteria w/ correction factors - this occurrs in dataPrep.
#' @param data A merged, parameter translated, spatially referenced WQP result object. Must be post parameter translation step.
#' @param crit_wb Full path and filename for workbook containing criteria.
#' @param crit_sheetname Name of sheet in workbook holding criteria to be assigned.
#' @param crit_startRow Row to start reading criteria table excel sheet from (in case headers have been added). Defaults to 1.
#' @param ss_sheetname Name of sheet in workbook holding site-specific criteria to be assigned. Should typically contain parameters corresponding to those in criteria table.
#' @param ss_startRow Row to start reading site specific criteria table excel sheet from (in case headers have been added). Defaults to 1.
#' @param rm_nocrit Logical. If TRUE (default), remove records w/o numeric criteria before returning. If FALSE, function will pass through all records with flattened uses & any matching criteria - useful for passing through non-assessed parameters for other analyses.

#' @return A flattened & expanded sample x use x parameter x criterion data.frame.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom reshape2 colsplit
#' @importFrom reshape2 melt
#' @importFrom plyr rbind.fill
#' @importFrom lubridate month
#' @export
assignCriteria=function(data, crit_wb, crit_sheetname, ss_sheetname, crit_startRow=1, ss_startRow=1, rm_nocrit=TRUE, print=TRUE){


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




#####Testing setup
#library(openxlsx)
#library(reshape2)
#library(plyr)
#data=acc_data
#data=within(data,{
#	ss_R317Descrp[ss_R317Descrp=='Lost Creek from the confluence with Sevier River to U.S. National Forest boundary']='Ivie Creek and its tributaries from the confluence with Muddy Creek to the confluence with Quitchupah Creek'
#})
#crit_wb="IR_uses_standards_working_v4_ef.xlsx"
#crit_sheetname="criteria"
#ss_sheetname="ss_criteria"
#crit_startRow=3
#ss_startRow=4
#any(data$R3172ParameterName=='Sulfate', na.rm=T)

#Check that data is post-parameter translation
if(!any(names(data)=="R3172ParameterName")){
	stop("Error: input data must be post-parameter translation and contain translated parameter names in R3172ParameterName column.")
	}
data=data[,!names(data) %in% "R3172ParameterName"]

#Load workbook
crit_wb=openxlsx::loadWorkbook(crit_wb)

#Read selected sheets in workbook table - note, these tables should be simplified/harmonized to just needed columns & eliminate overlap w/ parameter translation table (consider putting assessment & activity types here instead of param translation)
crit_table=data.frame(openxlsx::readWorkbook(crit_wb, sheet=crit_sheetname, startRow=crit_startRow, detectDates=TRUE))
ss_table=data.frame(openxlsx::readWorkbook(crit_wb, sheet=ss_sheetname, startRow=ss_startRow, detectDates=TRUE))

if(any(names(data) =='CAS_DWQ')){
	names(data)[names(data) =='CAS_DWQ']='CAS'
}

names(data)[names(data) %in% names(crit_table)]
names(data)[names(data) %in% names(ss_table)]


#Identify correction factor (CF) parameters in crit_table, append ",CF" to BEN_CLASS for all of these parameters (note, this is so target units can be defined for correction factors)
cf=crit_table[crit_table$BeneficialUse=="CF",]
data$BEN_CLASS[data$CAS %in% cf$CAS]=paste0(data$BEN_CLASS[data$CAS %in% cf$CAS],",CF")

#Identify supplemental parameters (SUP) parameters in crit_table, append ",SUP" to BEN_CLASS for all of these parameters (note, this is so target units can be defined for correction factors)
sup=crit_table[crit_table$BeneficialUse=="SUP",]
data$BEN_CLASS[data$CAS %in% sup$CAS]=paste0(data$BEN_CLASS[data$CAS %in% sup$CAS],",SUP")

#Expand comma separated uses (BEN_CLASS)
max_use_count=max(sapply(strsplit(data$BEN_CLASS,","),FUN="length"))
use_colnames=paste0(rep("use",max_use_count),seq(1:max_use_count))
uses_mat=unique(data.frame(data$BEN_CLASS,reshape2::colsplit(data$BEN_CLASS,",",use_colnames)))
names(uses_mat)[names(uses_mat)=="data.BEN_CLASS"]="BEN_CLASS"

#Flatten uses
uses_flat=reshape2::melt(uses_mat, id.vars="BEN_CLASS", value.name = "BeneficialUse")
uses_flat=uses_flat[,!names(uses_flat)=="variable"]
uses_flat=uses_flat[uses_flat$BeneficialUse!="" & !is.na(uses_flat$BeneficialUse),]

#Merge flat uses back to data by BEN_CLASS
data_uses_flat=merge(data,uses_flat,all=T)
rm(uses_flat)

#Merge criteria to data w/ flattened uses
data_uses_flat_crit=merge(data_uses_flat,crit_table,all.x=T)
dim_check=dim(data_uses_flat_crit)[1]
any(data_uses_flat_crit$R3172ParameterName=='Sulfate', na.rm=T) # no R3172P sulfate because no standards
any(data_uses_flat_crit$CAS=='14808-79-8', na.rm=T) # but sulfate exists in dataset

#Remove "CF" & SUP from comma-separated BEN_CLASS column (for future use in rollUp)
data_uses_flat_crit$BEN_CLASS=gsub(",CF","",data_uses_flat_crit$BEN_CLASS)
data_uses_flat_crit$BEN_CLASS=gsub(",SUP","",data_uses_flat_crit$BEN_CLASS)


#4. Site specific standards
#ID records w/ SS criteria (by SSCLocDescription)
#Remove those records from data_uses_flat_crit
ssc=unique(ss_table[,c("CAS", "BeneficialUse","ss_R317Descrp")])
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
dim(data_uses_flat_ssc)
rm(data_uses_flat)

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

any(data_uses_flat_crit$R3172ParameterName=='Sulfate', na.rm=T)
any(data_uses_flat_crit$CAS=='14808-79-8', na.rm=T)

#Remove records w/o criteria or not conversion factor if rm_nocrit==TRUE
if(rm_nocrit==TRUE){
	data_uses_flat_crit=data_uses_flat_crit[!is.na(data_uses_flat_crit$NumericCriterion) | data_uses_flat_crit$BeneficialUse=="CF" | data_uses_flat_crit$BeneficialUse=="SUP",]
}

# Show user table of parameters and the number of records with (and without) standards criteria for each use.
temp <- data_uses_flat_crit[!is.na(data_uses_flat_crit$NumericCriterion) | data_uses_flat_crit$BeneficialUse=="CF" |  data_uses_flat_crit$BeneficialUse=="SUP",]
temp1 <- data_uses_flat_crit[is.na(data_uses_flat_crit$NumericCriterion)& !data_uses_flat_crit$BeneficialUse=="CF" &  !data_uses_flat_crit$BeneficialUse=="SUP",]
if(print){
	print("Data record counts for each parameter with standards criteria and associated beneficial use:")
	print(table(temp$R3172ParameterName, temp$BeneficialUse))
	#if(length(temp1[,1])>0){
	#print("Data record counts for each parameter without standard criteria:")
	#print(table(temp1$R3172ParameterName, temp1$BeneficialUse))
	#}
}

return(data_uses_flat_crit)


}
