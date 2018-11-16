#' Calculate sample counts and exceedance counts and frequencies
#'
#' Compares water quality result values to standards to calculates sample counts, exceedance counts, and exceedance frequencies by grouping variables.
#' 
#' @param data A prepped water quality portal data object (i.e. output from dataPrep() )
#' @param group_vars Vector of column names on which to group data when calculating sample counts and exceedances

#' @return Returns sample counts, exceedance counts, and exceedance frequencies aggregated by grouping variables.


#' @export
#countExceedances=function(data, group_vars){
	
#Set up
data=conventionals
data$CriterionType[is.na(data$CriterionType)]="max"
group_vars=c("IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","BeneficialUse","R3172ParameterName","CriterionLabel","CriterionType","SSC_MLID")

#Make sure NumericCriterion is numeric class
facToNum=function(x){return(as.numeric(levels(x))[x])}
if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=facToNum(data$NumericCriterion)}

#Mark exceedances w/ 1, non-exceedances w/0
data_exc=data
data_exc$exc=0
data_exc=within(data_exc, {
	exc[CriterionType=="max" & IR_Value > NumericCriterion]=1
	exc[CriterionType=="min" & IR_Value < NumericCriterion]=1
	})
table(data_exc$exc)


addNA_fac=function(x){
	if(class(x)=="factor"){
		y=addNA(x,ifany=T)
	}else{y=x}
	return(y)
}





#Subset to group_vars, then aggregate for samp & exc counts
data_exc=data_exc[,names(data_exc) %in% group_vars | names(data_exc) %in% "exc"]
data_exc=as.data.frame(lapply(data_exc, addNA_fac)) #Add NA as factor level where cols contain NAs (converts everything to factor)

samp_count=aggregate(exc~., data_exc, FUN="length")
names(samp_count)[names(samp_count) %in% "exc"]="SampleCount"
table(samp_count$SampleCount)
samp_count[samp_count$SampleCount==314,]


exc_count=aggregate(exc~IR_MLID+ActivityStartDate+R317Descrp+IR_Lat+IR_Long+ASSESS_ID+BeneficialUse+R3172ParameterName+CriterionLabel+CriterionType+SSC_MLID,data_exc, FUN="sum")
	
	
	
	
#}
