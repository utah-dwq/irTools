#' Perform assessments based on sample & exceedance counts (conventional & toxic assessments)
#'
#' Performs site level (site-parameter-use-criterion specific) water quality assessments based on sample & exceedance counts by applying IR method flowcharts with user specified inputs.
#' 
#' When percentage arguments are supplied, specified percentages are used to calculate the maximum allowable number of whole samples exceeding a standard for that site/parameter to be considered supporting. Note that the min_n argument is >= and all other arguments are >.
#' @param data Site-use-parameter-criterion specific sample and  exceedance counts(i.e. output from countExceedances() )
#' @param min_n Minimum sample size for sufficient data assessment (sample counts >= min_n are considered sufficient)
#' @param max_exc_count Maximum allowable exceedance count for full support (exceedance counts > max_exc_count are considered not supporting) - one of max_exc_count or max_exc_pct must be specified
#' @param max_exc_pct Maximum allowable exceedance percentage for full support (exceedance pcts > max_exc_pct are considered not supporting - one of max_exc_count or max_exc_pct must be specified
#' @param max_exc_count_id Maximum allowable exceedance count for insufficient data with exceedances (for sites/parameters with insufficient data, exceedance counts > max_exc_count_id are considered insufficient data with exceedances) - one of max_exc_count_id or max_exc_pct_id must be specified
#' @param max_exc_pct_id Maximum allowable exceedance percentage for insufficient data with exceedances (for sites/parameters with insufficient data, exceedance pcts > max_exc_count_id are considered insufficient data with exceedances) - one of max_exc_count_id or max_exc_pct_id must be specified

#' @return Returns input data frame with site-use-parameter-criterion specific assessment categories appended.


#' @export
assessExcCounts=function(data, min_n, max_exc_count=NA, max_exc_pct=NA, max_exc_count_id=NA, max_exc_pct_id=NA, id_cols=c()){

##Set up####
#data=conv_exc
#min_n=10
#
#max_exc_count=NA
#max_exc_pct=10
#
#max_exc_count_id=1
#max_exc_pct_id=NA
#########

	#Check args
	if(is.na(max_exc_count) & is.na(max_exc_pct)){stop("Error: one of max_exc_count or max_exc_pct must be supplied.")}
	if(!is.na(max_exc_count) & !is.na(max_exc_pct)){stop("Error: Only one of max_exc_count or max_exc_pct may be supplied.")}
	if(is.na(max_exc_count_id) & is.na(max_exc_pct_id)){stop("Error: one of max_exc_count_id or max_exc_pct_id must be supplied.")}
	if(!is.na(max_exc_count_id) & !is.na(max_exc_pct_id)){stop("Error: Only one of max_exc_count_id or max_exc_pct_id may be supplied.")}
	
	data$IR_Cat=NA
	
	# rounding = function(x,max_exc_pct){
	#   y = x*max_exc_pct/100+0.5
	#   z = floor(y)
	#   return(z)
	# }
	
	
	# data=within(data,{
	# 	max_allow_exc=rounding(SampleCount,max_exc_pct) #Note, will produce all NAs if is.na(max_exc_pct)
	# 	max_allow_exc_id=ceiling(SampleCount*max_exc_pct_id/100) #Note, will produce all NAs if is.na(max_exc_pct_id)
	# })
	
	if(!is.na(max_exc_pct)){ #Exceedance pct based sufficient data assessments
		if(!is.na(max_exc_count_id)){ #Exceedance count based insufficient data assessments
			data=within(data,{
			  Percent_Exceed = NA
				IR_Cat[SampleCount>=min_n & (ExcCount/SampleCount)>max_exc_pct]="NS"
				IR_Cat[SampleCount>=min_n & (ExcCount/SampleCount)<=max_exc_pct]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_exc_count_id]="IDEX"
				IR_Cat[SampleCount<min_n & ExcCount<=max_exc_count_id]="IDNE"
			})
		}else{  #Exceedance pct based insufficient data assessments
			data=within(data,{
			  Percent_Exceed = NA
				IR_Cat[SampleCount>=min_n & (ExcCount/SampleCount)>max_exc_pct]="NS"
				IR_Cat[SampleCount>=min_n & (ExcCount/SampleCount)<=max_exc_pct]="FS"
				IR_Cat[SampleCount<min_n & (ExcCount/SampleCount)>max_exc_pct_id]="IDEX"
				IR_Cat[SampleCount<min_n & (ExcCount/SampleCount)<=max_exc_pct_id]="IDNE"
			})
		}
	  data$Percent_Exceed = ifelse(data$SampleCount>=min_n, data$ExcCount/data$SampleCount, NA)
	}else{ #Exceedance count based assessments
		if(!is.na(max_exc_count_id)){ #Exceedance count based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_exc_count]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_exc_count]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_exc_count_id]="IDEX"
				IR_Cat[SampleCount<min_n & ExcCount<=max_exc_count_id]="IDNE"
			})
		}else{  #Exceedance pct based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_exc_count]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_exc_count]="FS"
				IR_Cat[SampleCount<min_n & (ExcCount/SampleCount)>max_exc_pct_id]="IDEX"
				IR_Cat[SampleCount<min_n & (ExcCount/SampleCount)<=max_exc_pct_id]="IDNE"
			})
		}
	}

return(data)

}
