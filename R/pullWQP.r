#' Pull water quality, station, and other data for Utah from USEPA WQP
#'
#' Pulls data from EPA's Water Quality Portal (WQP) and exports data files (as .csv) to outfile_path folder.
#' Prepared by C Shope 1/25/18, Adapted from Roop Guha-NJDEP 1/3/18.
#' Updated, re-formated to function, & packaged by J VanderLaan.
#' @param outfile_path Path for file outputs.
#' @param StartDate Query start date. "MM-DD-YYYY" format.
#' @param EndDate Query end date. "MM-DD-YYYY" format.
#' @param retrieve Vector of data type names to retrieve from WQP. One or more of: "result","narrowresult","activity","activitymetric","sites","detquantlim". Defaults to query all.
#' @return Exports .csv files for all selected data types during selected date period in specified output path.

#' @export
pullWQP<-function(outfile_path,StartDate,EndDate,retrieve=c("narrowresult","activity","sites","detquantlim"),retry=FALSE){

startlab = format(strptime(StartDate, "%m-%d-%Y"),"%y%m%d") # set label as yymmdd for file identifier
endlab = format(strptime(EndDate, "%m-%d-%Y"),"%y%m%d") # set label as yymmdd for file identifier


####
#alternative option to skip/replace download read.csv("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=01-01-2010&startDateHi=01-01-2018&mimeType=csv&zip=no&sorted=no&dataProfile=narrowResult&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
####



#Set everything inside while + try loop to check for existence of desired files (generate file name list first) - occasionally the connection gets timed out, this lets R keep trying until all files are downloaded even if time out occurs
file_list=vector(length=length(retrieve))
for(n in 1:length(retrieve)){
	file_name_n=paste(outfile_path,"\\",retrieve[n],startlab,"-",endlab,".csv",sep="")
	file_list[n]=file_name_n
	}

if(retry==TRUE){
	while(any(file.exists(file_list)==FALSE)){
	try({
	## GET THE STATION DATA DOWNLOADED (JV - I've found station pulls to work best straight to csv w/o zipping.)
	# Get the WQP station data with given date range
		if("sites" %in% retrieve & !file.exists(paste0(outfile_path,"\\sites",startlab,"-",endlab,".csv",sep=""))){
			StnSource=paste0("https://www.waterqualitydata.us/Station/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
			# Set output location for the WQP station data, unzip, and rename
			StnZipDest = paste0(outfile_path,"\\sites",startlab,"-",endlab,".csv")
			download.file(StnSource,StnZipDest) # download file to local
		}
	
	## GET THE WATER QUALITY DATA DOWNLOADED (wide and narrow result files)
	# Get the WQP result data with given date range
	# Set output location for the WQP results data, unzip, and rename
	if("result" %in% retrieve & !file.exists(paste0(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))){
	WQSource = paste0("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	WQZipDest = paste0(outfile_path,"\\WQ.zip")
	WQDest = outfile_path
	download.file(WQSource,WQZipDest,method = "auto", mode = "wb") # download file to local
	unzip(WQZipDest,exdir = WQDest) # unzip file to local
	fn = paste0(outfile_path,"\\result.csv")
	if (file.exists(fn)){ file.rename(fn, paste(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))}# rename file
	FN = (paste0(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))
	if (file.exists(FN)){file.remove(WQZipDest)} # Get rid of the .zip file
		}
	if("narrowresult" %in% retrieve & !file.exists(paste0(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))){ 
	WQSource = paste0("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&dataProfile=narrowResult&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	WQZipDest = paste0(outfile_path,"\\WQ.zip")
	#WQSource = paste0("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&sorted=no&dataProfile=narrowResult")
	#WQZipDest = paste0(outfile_path,"\\narrowresult.csv")
	WQDest = outfile_path
	download.file(WQSource,WQZipDest,method = "auto", mode = "wb") # download file to local
	unzip(WQZipDest,exdir = WQDest) # unzip file to local
	fn = paste0(outfile_path,"\\narrowresult.csv")
	if (file.exists(fn)){ file.rename(fn, paste(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))}# rename file
	FN = (paste0(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))
	if (file.exists(FN)){file.remove(WQZipDest)} # Get rid of the .zip file
		}
	
	
	## GET THE SAMPLING ACTIVITY DATA DOWNLOADED
	# Get the WQP sampling activities data with given date range
	# Set output location for the WQP activity data, unzip, and rename
	if("activity" %in% retrieve & !file.exists(paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv",sep=""))){
	ActSource = paste0("https://www.waterqualitydata.us/Activity/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	ActZipDest = paste0(outfile_path,"\\Activity.zip")
	ActDest = outfile_path
	download.file(ActSource,ActZipDest,method = "auto", mode = "wb") # download file to local
	unzip(ActZipDest,exdir = ActDest) # unzip file to local
	sn = paste0(outfile_path,"\\activity.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv")
	if (file.exists(SN)){ file.remove(ActZipDest)}# Get rid of the .zip file
		}
	
	## GET THE SAMPLING ACTIVITY METRICS DATA DOWNLOADED
	# Get the WQP sampling activity metrics data with given date range
	# Set output location for the WQP activity metric data, unzip, and rename
	if("activitymetric" %in% retrieve & !file.exists(paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv",sep=""))){
	ActMetSource = paste0("https://www.waterqualitydata.us/ActivityMetric/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	ActMetZipDest = paste0(outfile_path,"\\ActivityMetric.zip")
	ActMetDest = outfile_path
	download.file(ActMetSource,ActMetZipDest,method = "auto", mode = "wb") # download file to local
	unzip(ActMetZipDest,exdir = ActMetDest) # unzip file to local
	sn = paste0(outfile_path,"\\activitymetric.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv",sep="")
	if (file.exists(SN)){file.remove(ActMetZipDest)}# Get rid of the .zip file
		}
	
	## GET THE RESULT QUANTITATION DATA DOWNLOADED
	# Get the WQP result quantitation data with given date range
	# Set output location for the WQP result quantitation data, unzip, and rename
	
	if("detquantlim" %in% retrieve & !file.exists(paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv",sep=""))){
	QuantLimSource = paste0("https://www.waterqualitydata.us/ResultDetectionQuantitationLimit/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	QuantLimZipDest = paste0(outfile_path,"\\ResDetectQuantLim.zip")
	QuantLimDest = outfile_path
	download.file(QuantLimSource,QuantLimZipDest,method = "auto", mode = "wb") # download file to local
	unzip(QuantLimZipDest,exdir = QuantLimDest) # unzip file to local
	sn = paste0(outfile_path,"\\resdetectqntlmt.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv",sep="")
	if (file.exists(SN)){file.remove(QuantLimZipDest)} # Get rid of the .zip file
		}
})
}
}else{
	try({
	## GET THE STATION DATA DOWNLOADED (JV - I've found station pulls to work best straight to csv w/o zipping.)
	# Get the WQP station data with given date range
		if("sites" %in% retrieve & !file.exists(paste0(outfile_path,"\\sites",startlab,"-",endlab,".csv",sep=""))){
			StnSource=paste0("https://www.waterqualitydata.us/Station/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=no&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
			# Set output location for the WQP station data, unzip, and rename
			StnZipDest = paste0(outfile_path,"\\sites",startlab,"-",endlab,".csv")
			download.file(StnSource,StnZipDest) # download file to local
		}
	
	## GET THE WATER QUALITY DATA DOWNLOADED (wide and narrow result files)
	# Get the WQP result data with given date range
	# Set output location for the WQP results data, unzip, and rename
	if("result" %in% retrieve & !file.exists(paste0(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))){
	WQSource = paste0("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	WQZipDest = paste0(outfile_path,"\\WQ.zip")
	WQDest = outfile_path
	download.file(WQSource,WQZipDest,method = "auto", mode = "wb") # download file to local
	unzip(WQZipDest,exdir = WQDest) # unzip file to local
	fn = paste0(outfile_path,"\\result.csv")
	if (file.exists(fn)){ file.rename(fn, paste(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))}# rename file
	FN = (paste0(outfile_path,"\\result",startlab,"-",endlab,".csv",sep=""))
	if (file.exists(FN)){file.remove(WQZipDest)} # Get rid of the .zip file
		}
	if("narrowresult" %in% retrieve & !file.exists(paste0(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))){
	WQSource = paste0("https://www.waterqualitydata.us/Result/search?&statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&dataProfile=narrowResult&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	WQZipDest = paste0(outfile_path,"\\WQ.zip")
	WQDest = outfile_path
	download.file(WQSource,WQZipDest,method = "auto", mode = "wb") # download file to local
	unzip(WQZipDest,exdir = WQDest) # unzip file to local
	fn = paste0(outfile_path,"\\narrowresult.csv")
	if (file.exists(fn)){ file.rename(fn, paste(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))}# rename file
	FN = (paste0(outfile_path,"\\narrowresult",startlab,"-",endlab,".csv",sep=""))
	if (file.exists(FN)){file.remove(WQZipDest)} # Get rid of the .zip file
		}
	
	
	## GET THE SAMPLING ACTIVITY DATA DOWNLOADED
	# Get the WQP sampling activities data with given date range
	# Set output location for the WQP activity data, unzip, and rename
	if("activity" %in% retrieve & !file.exists(paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv",sep=""))){
	ActSource = paste0("https://www.waterqualitydata.us/Activity/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	ActZipDest = paste0(outfile_path,"\\Activity.zip")
	ActDest = outfile_path
	download.file(ActSource,ActZipDest,method = "auto", mode = "wb") # download file to local
	unzip(ActZipDest,exdir = ActDest) # unzip file to local
	sn = paste0(outfile_path,"\\activity.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\activity",startlab,"-",endlab,".csv")
	if (file.exists(SN)){ file.remove(ActZipDest)}# Get rid of the .zip file
		}
	
	## GET THE SAMPLING ACTIVITY METRICS DATA DOWNLOADED
	# Get the WQP sampling activity metrics data with given date range
	# Set output location for the WQP activity metric data, unzip, and rename
	if("activitymetric" %in% retrieve & !file.exists(paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv",sep=""))){
	ActMetSource = paste0("https://www.waterqualitydata.us/ActivityMetric/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	ActMetZipDest = paste0(outfile_path,"\\ActivityMetric.zip")
	ActMetDest = outfile_path
	download.file(ActMetSource,ActMetZipDest,method = "auto", mode = "wb") # download file to local
	unzip(ActMetZipDest,exdir = ActMetDest) # unzip file to local
	sn = paste0(outfile_path,"\\activitymetric.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\activitymetric",startlab,"-",endlab,".csv",sep="")
	if (file.exists(SN)){file.remove(ActMetZipDest)}# Get rid of the .zip file
		}
	
	## GET THE RESULT QUANTITATION DATA DOWNLOADED
	# Get the WQP result quantitation data with given date range
	# Set output location for the WQP result quantitation data, unzip, and rename
	
	if("detquantlim" %in% retrieve & !file.exists(paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv",sep=""))){
	QuantLimSource = paste0("https://www.waterqualitydata.us/ResultDetectionQuantitationLimit/search?statecode=US%3A49&startDateLo=",StartDate,"&startDateHi=",EndDate,"&mimeType=csv&zip=yes&sorted=no&siteType=Aggregate%20surface-water-use&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Spring&siteType=Stream")
	QuantLimZipDest = paste0(outfile_path,"\\ResDetectQuantLim.zip")
	QuantLimDest = outfile_path
	download.file(QuantLimSource,QuantLimZipDest,method = "auto", mode = "wb") # download file to local
	unzip(QuantLimZipDest,exdir = QuantLimDest) # unzip file to local
	sn = paste0(outfile_path,"\\resdetectqntlmt.csv")
	if (file.exists(sn)){file.rename(sn,paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv"))} # rename file
	SN = paste0(outfile_path,"\\detquantlim",startlab,"-",endlab,".csv",sep="")
	if (file.exists(SN)){file.remove(QuantLimZipDest)} # Get rid of the .zip file
		}
	})
}


}



