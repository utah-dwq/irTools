#' Calculate sample and exceedance counts (conventional & toxic assessments)
#'
#' Compares water quality result values to standards to calculates sample and exceedance counts. This is geared towards conventional and toxic assessments.
#' 
#' @param data A prepped water quality portal data object (i.e. output from dataPrep() )
#' @param group_vars Vector of column names on which to group data when calculating sample counts and exceedances. This should not include any factors that prevent aggregation to site-scale assessments (e.g. date, time, etc.). See default for recommended.

#' @return Returns sample and exceedance counts aggregated by grouping variables.


#' @export
countExceedances=function(data, group_vars=c("IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","R3172ParameterName","CriterionLabel","SSC_MLID","AsmntAggFun")){
	
###Set up
#data=conventionals
#data$CriterionType[is.na(data$CriterionType)]="max"
#data=data[data$BeneficialUse!="CF",]
#group_vars=c("IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","R3172ParameterName","CriterionLabel","SSC_MLID","AsmntAggFun")

if(any(is.na(data$CriterionType))){stop("Error: NA values in criterion type (min or max). Standards table update required")}

#Make sure NumericCriterion is numeric class

facToNum=function(x){
	if(class(x)=="factor"){result=as.numeric(levels(x))[x]
	}else{result=x}
	return(result)
	}

if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=facToNum(data$NumericCriterion)}

#Mark exceedances w/ 1, non-exceedances w/0
data_exc=data
data_exc$exc=0
data_exc=within(data_exc, {
	exc[CriterionType=="max" & IR_Value > NumericCriterion]=1
	exc[CriterionType=="min" & IR_Value < NumericCriterion]=1
	})
table(data_exc$exc)

#addNA to factor type columns function
addNA_fac=function(x){
	if(class(x)=="factor"){
		y=addNA(x,ifany=T)
	}else{y=x}
	return(y)
}

#Generate sample counts (length of IR_Value in unique data_exc[,group_vars])
data_samps=unique(data_exc[,names(data_exc) %in% group_vars | names(data_exc) %in% c("IR_Value","ActivityStartDate")])
data_samps=data_samps[, !names(data_samps) %in% "ActivityStartDate"]
data_samps=as.data.frame(lapply(data_samps, addNA_fac)) #Add NA as factor level where cols contain NAs (converts everything to factor)
samp_count=aggregate(IR_Value~., data_samps, FUN="length")
names(samp_count)[names(samp_count) %in% "IR_Value"]="SampleCount"
table(samp_count$SampleCount)

#Subset to group_vars, then aggregate for exc counts
data_exc=data_exc[,names(data_exc) %in% group_vars | names(data_exc) %in% "exc"]
data_exc=as.data.frame(lapply(data_exc, addNA_fac)) #Add NA as factor level where cols contain NAs (converts everything to factor)

exc_count=aggregate(exc~., data_exc, FUN="sum")
names(exc_count)[names(exc_count) %in% "exc"]="ExcCount"
table(exc_count$ExcCount)

dim(samp_count)[1]==dim(exc_count)[1]
samp_exc=merge(samp_count, exc_count)
dim(samp_exc)[1]==dim(samp_count)[1]

#Manually checking a couple of things...
#samp_exc[samp_exc$ExcCount==19,]
#samp_exc[samp_exc$IR_MLID=="UTAHDWQ_WQX-5994740" & samp_exc$R3172ParameterName=="pH" & samp_exc$BeneficialUse=="2B",]

#samp_exc=within(samp_exc,{ExcFrq=ExcCount/SampleCount})

return(samp_exc)
	
}







