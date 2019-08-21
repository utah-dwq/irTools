#' Run lake dashboard shiny app

#' Runs lake dashboard shiny app embedded in UT IR tools package.
#' @importFrom dplyr rename
#' @export
lakeDashboard=function(){
	 shiny::runApp(system.file('lakeDashboard', package='irTools'))
	 }
