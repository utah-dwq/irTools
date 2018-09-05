#' Pull data from EPA's ATTAINS database
#'
#' This function pulls information from EPA ATTAINS database based on submitted arguments via ATTAINS web service. Any ATTAINS web service compatible argument can be submitted to this funciton. Depending on type, at least one argument may be required. See https://link-to-be-obtained.gov for a list of required and compatible arguments.
#' The function is essentially an ATTAINS specific wrapper for jsonlite::fromJSON. It generates the appropriate web path to connect to ATTAINS web service and converts JSON to R object.
#'
#' @param type ATTAINS data type to pull. One of: "assessmentUnits", "assessments", "actions", or "domains".
#' @param stateCode Two letter state code (for all pulls where type != "assessments")
#' @param state Two letter state code (for pull type == "assessments")
#' @param ... Additional arguments to be passed to ATTAINS web service path.

#' @importFrom jsonlite fromJSON

#' @return Returns a data.frame of queried ATTAINS data. Note that some results data.frames may contain additional lists of data.frames that can be further flattened.

#' @examples
#' #Pull South Dakota assessment unit information
#' SD_AUs=pullAttains(type="assessmentUnits", stateCode="SD")
#' 
#' #Pull South Dakota actions
#' SD_actions=pullAttains(type="actions", stateCode="SD")

#' @export
pullAttains=function(type="assessments", ...){

path="https://attains.epa.gov/attains-public/api/"
args=list(...)
#print(args)
#print(length(args))
path=paste0(path, type, "?")
if(length(args)>0){
	for(n in 1:length(args)){
		if(n<length(args)){
			arg_n=paste0(names(args[n]),"=",args[n],"&")
			path=paste0(path,arg_n)
		}
		if(n==length(args)){
			arg_n=paste0(names(args[n]),"=",args[n])
			path=paste0(path,arg_n)
		}
	}
}

result=fromJSON(path, flatten=T)$items
return(result)
print(paste("Data pulled from: ", path))

}



