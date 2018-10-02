#' Assign numeric criteria
#'
#' Assigns numeric WQ criteria to WQP data. 
#' @param data A merged, parameter translated, spatially referenced WQP result object.
#' @param crit_wb Full path and filename for workbook containing criteria.
#' @param sheetname Name of sheet in workbook holding criteria to be assigned.
#' @param startRow Row to start reading excel sheet from (in case headers have been added). Defaults to 1.
#' @return A flattened & expanded sample x use x parameter x criterion data.frame.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom reshape2 colsplit
#' @export
assignCriteria=function(data, translation_wb, sheetname, startRow=1, browsefile=F){ 


####Process
###Assign numeric criteria to WQP data

###Inputs:
#1. Merged, translated, & spatially validated data (i.e. narrow result + activity file w/ all screen tables including sites applied)
#2. Criterion file (use x parameter)

####Rough outline:
#1. Flatten data by use
#	-Expand ben use col (BEN_CLASS = BUs assigned spatially as comma separated vector in single column)
#	-Flatten
#2. Read in criterion table
#3. Merge criterion table to data (by BU + R3172ParameterName, all.x=T)
#4. Site specific standards
#	-ID records w/ SS criteria
#	-Remove those records from data_uses_flat_crit
#	-Merge SS criteria to data_uses_flat (only keep matches)
#	-Remove matches where MLID != SS MLID (if provided)
#	-rbind matches back to data_uses_flat_crit
#5. Calculated criteria (correction factors)


###Testing setup
#library(openxlsx)
#library(reshape2)
#data=mrf_sub
#crit_wb="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\04standards\\IR_uses_standards.xlsx"
#sheetname="R317214DomesticRecAgCriteria"
#startRow=1

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


#Load workbook
crit_wb=loadWorkbook(crit_wb)

#Read selected sheet in workbook table
crit_table=data.frame(readWorkbook(crit_wb, sheet=sheetname, startRow=startRow, detectDates=TRUE))

#Merge criteria to data w/ flattened uses
data_uses_flat_crit=merge(data_uses_flat,crit_table,by.x=c("R3172ParameterName", "BeneficialUse"),by.y=c("Parameter","BeneficialUse"),all.x=T)


#4. Site specific standards
#	-ID records w/ SS criteria
#	-Remove those records from data_uses_flat_crit
#	-Merge SS criteria to data_uses_flat (only keep matches)
#	-Remove matches where MLID != SS MLID (if provided)
#	-rbind matches back to data_uses_flat_crit


#5. Calculated criteria - JV this may actually be better done separately (prob stand-alone function), following unit conversions to make sure all of the correction factor parameters are in consistent units. Steps should be the same.
#-Grab records that need criteria calculated (remove these from data?)
#-Build correction factor subdataset
#-Cast correction factor subdataset
#-Merge correction factors to records needing calculated criteria
#-Apply formulas to calculate criteria
#-rbind corrections back to data (keep columns of correction factors, fill w/ NA for other data)


return(data_uses_flat_crit)


}
