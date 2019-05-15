#' Run the assessment dashboard application

#' Runs the assessment dashboard shiny app embedded in UT IR tools package.

#' @import shiny
#' @import wqTools
#' @import leaflet
#' @import shinyBS
#' @import plotly

#' @export
asmntDashboard=function(){
	shiny::runApp(system.file('asmntDashboard', package='irTools'))
}
