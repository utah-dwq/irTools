#' Run site validation shiny app

#' Runs site validation shiny app embedded in UT IR tools package.

#' @import shiny
#' @importFrom wqTools buildMap

#' @importFrom plyr rbind.fill
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom DT renderDT
#' @importFrom DT datatable

#' @export
runSiteValApp=function(){
	 shiny::runApp(system.file('siteValApp', package='irTools'))
	 }
