#' Calculate sample counts and exceedance counts and frequencies
#'
#' Compares water quality result values to standards to calculates sample counts, exceedance counts, and exceedance frequencies by grouping variables.
#' 
#' @param data A prepped water quality portal data object (i.e. output from dataPrep() )
#' @param group_vars Vector of column names on which to group data when calculating sample counts and exceedances

#' @return Returns sample counts, exceedance counts, and exceedance frequencies aggregated by grouping variables.


#' @export
countExceedances=function(data, group_vars){
	
#Set up
data=conventionals
group_vars=c("")


#Mark exceedances w/ 1, non-exceedances w/0
data$exc=NA
within(data,{
	

	})	



	}
