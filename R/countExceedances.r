#' Calculate sample and exceedance counts (conventional & toxic assessments)
#'
#' Compares water quality result values to standards to calculates sample and exceedance counts. This is geared towards conventional and toxic assessments.
#' 
#' @param data A prepped water quality portal data object (i.e. output from dataPrep() ). Must include IR_Value and ActivityStartDate.
#' @param group_vars Vector of column names on which to group data when calculating sample counts and exceedances. This should not include any factors that prevent aggregation to site-scale assessments (e.g. date, time, etc.), but should include any columns that indicate a unique standard (e.g. season, acute v. chronic, etc.). See default for recommended.
#' @param agg_exc Logical. If FALSE (default), individual samples are compared to the criterion as with non-aggregated criteria to count exceedances. If TRUE (default) aggregate samples by AsmntAggFun prior to counting exceedances.
#' @param agg_exc_as_n Logical. If FALSE exceedance/support of aggregate water quality criteria (e.g. seasonal means) is indicated as 1 or 0 in ExcCount column. Only used if agg_exc==TRUE. If TRUE (default), set the ExcCount value of aggregate water quality criteria to equal the associated sample count. In this case, ExcCount==SampleCount indicates an exceedance and ExcCount==0 indicates no exceedance. This allows sample and exceedance counts for these types of criteria to pass through assessExc using the same input arguments.

#' @return Returns sample and exceedance counts aggregated by grouping variables.
#' @importFrom plyr rbind.fill
#' @importFrom wqTools facToNum

#' @export
countExceedances=function(data, group_vars=c("IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","BEN_CLASS","R3172ParameterName","CriterionLabel","SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun"), agg_exc=FALSE, agg_exc_as_n=TRUE){
	
###Set up
#data=conventionals
#data$CriterionType[is.na(data$CriterionType)]="max"
#data=data[data$BeneficialUse!="CF",]
#group_vars=c("IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","R3172ParameterName","CriterionLabel","SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun")
#agg_exc=TRUE
#agg_exc_as_n=TRUE

#Set class for all group_vars to factor
data[,names(data) %in% group_vars]=as.data.frame(lapply(data[,names(data) %in% group_vars], as.factor))

if(any(is.na(data$CriterionType[data$BeneficialUse!="CF"]))){stop("Error: NA values in criterion type (min or max). Standards table update required")}

#addNA to factor type columns function
addNA_fac=function(x){
	if(class(x)=="factor"){
		y=addNA(x,ifany=T)
	}else{y=x}
	return(y)
}

	
if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=wqTools::facToNum(data$NumericCriterion)}


#Generate sample counts (length of IR_Value in unique data_exc[,group_vars])
data_samps=unique(data[,names(data) %in% group_vars | names(data) %in% c("IR_Value","ActivityStartDate")])
data_samps=data_samps[, !names(data_samps) %in% "ActivityStartDate"]
data_samps=as.data.frame(lapply(data_samps, addNA_fac)) #Add NA as factor level where cols contain NAs (converts everything to factor)
samp_count=aggregate(IR_Value~., data_samps, FUN="length")
names(samp_count)[names(samp_count) %in% "IR_Value"]="SampleCount"
table(samp_count$SampleCount)


#x=asmnt_agg_data
#value_var="IR_Value"
#agg_var="AsmntAggFun"
#drop_vars="ActivityStartDate"
#Aggregate by function vector
aggbyfun=function(x, value_var, drop_vars, agg_var){
	val=x[,value_var]
	x=x[,!names(x) %in% value_var & !names(x) %in% drop_vars]
	num_names=names(x[unlist(lapply(x, is.numeric))])
	x=as.data.frame(lapply(x, addNA, ifany=T)) #Add NA as factor level where cols contain NAs (converts everything to factor)
	x=data.frame(val,x) #Add back in preserved numeric val (alternatively could allow it to convert to factor then use as.numeric(levels(z))[z])
	x=x[,!names(x) %in% drop_vars]
	agg=x[0,]
	funs=unique(x[,agg_var])
	
	for(n in 1:length(funs)){
		fun_n=funs[n]
		x_n=x[x[,agg_var]==fun_n,]
		agg_n=aggregate(val~.,x_n, FUN=get(paste(fun_n)))
		agg=rbind(agg,agg_n)
	}
	
	agg[num_names]=lapply(agg[num_names], wqTools::facToNum) #Convert numeric cols back to numeric
	
	names(agg)[names(agg)=="val"]=value_var #Rename value_var
	
	return(agg)
}


###Aggregate by AsmntAggFun ( if !is.na() )
if(agg_exc){
	asmnt_agg_data=data[!is.na(data$AsmntAggFun),] #Subset to records that need aggregation
	if(dim(asmnt_agg_data)[1]>0){
		data=data[is.na(data$AsmntAggFun),] #remove those records from data
		aggregated=aggbyfun(asmnt_agg_data,value_var="IR_Value",agg_var="AsmntAggFun",drop_vars="ActivityStartDate")
		data=plyr::rbind.fill(data,aggregated)
	}
	rm(asmnt_agg_data)
}

#Mark exceedances w/ 1, non-exceedances w/0
data_exc=data
data_exc$exc=0
data_exc=within(data_exc, {
	exc[CriterionType=="max" & IR_Value > NumericCriterion]=1
	exc[CriterionType=="min" & IR_Value < NumericCriterion]=1
	})
table(data_exc$exc)


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

if(agg_exc & agg_exc_as_n){
	samp_exc=within(samp_exc,{
		ExcCount[!is.na(as.character(AsmntAggFun)) & ExcCount>0]=SampleCount[!is.na(as.character(AsmntAggFun)) & ExcCount>0]
	})
}

samp_exc[!is.na(as.character(samp_exc$AsmntAggFun)),]


return(samp_exc)
	
}







