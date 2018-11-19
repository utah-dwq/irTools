#' Perform assessments based on sample & exceedance counts
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
#max_exc_count=2
#max_exc_pct=NA
#
#max_exc_count_id=NA
#max_exc_pct_id=10
#########

	#Check args
	if(is.na(max_exc_count) & is.na(max_exc_pct)){stop("Error: one of max_exc_count or max_exc_pct must be supplied.")}
	if(!is.na(max_exc_count) & !is.na(max_exc_pct)){stop("Error: Only one of max_exc_count or max_exc_pct may be supplied.")}
	if(is.na(max_exc_count_id) & is.na(max_exc_pct_id)){stop("Error: one of max_exc_count_id or max_exc_pct_id must be supplied.")}
	if(!is.na(max_exc_count_id) & !is.na(max_exc_pct_id)){stop("Error: Only one of max_exc_count_id or max_exc_pct_id may be supplied.")}
	
	data$IR_Cat=NA
	
	
	data=within(data,{
		max_allow_exc=ceiling(data$SampleCount*max_exc_pct/100) #Note, will produce all NAs if is.na(max_exc_pct)
		max_allow_exc_id=ceiling(SampleCount*max_exc_pct_id/100) #Note, will produce all NAs if is.na(max_exc_pct_id)
	})
	
	if(!is.na(max_exc_pct)){ #Exceedance pct based sufficient data assessments
	
		if(!is.na(max_exc_count_id)){ #Exceedance count based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_allow_exc]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_allow_exc]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_exc_count_id]="idE"
				IR_Cat[SampleCount<min_n & ExcCount<=max_exc_count_id]="idNE"
			})
		}else{  #Exceedance pct based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_allow_exc]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_allow_exc]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_allow_exc_id]="idE"
				IR_Cat[SampleCount<min_n & ExcCount<=max_allow_exc_id]="idNE"
			})
		}

	}else{ #Exceedance count based assessments
		if(!is.na(max_exc_count_id)){ #Exceedance count based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_exc_count]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_exc_count]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_exc_count_id]="idE"
				IR_Cat[SampleCount<min_n & ExcCount<=max_exc_count_id]="idNE"
			})
		}else{  #Exceedance pct based insufficient data assessments
			data=within(data,{
				IR_Cat[SampleCount>=min_n & ExcCount>max_exc_count]="NS"
				IR_Cat[SampleCount>=min_n & ExcCount<=max_exc_count]="FS"
				IR_Cat[SampleCount<min_n & ExcCount>max_allow_exc_id]="idE"
				IR_Cat[SampleCount<min_n & ExcCount<=max_allow_exc_id]="idNE"
			})
		}
	}

data=data[,!names(data) %in% c("max_allow_exc_id","max_allow_exc")]
return(data)

}
