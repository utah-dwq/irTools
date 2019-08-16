#' Run lake dashboard shiny app

#' Runs lake dashboard shiny app embedded in UT IR tools package.

#' @export
lakeDashboard=function(){
	 shiny::runApp(system.file('lakeDashboard', package='irTools'))
	 }
