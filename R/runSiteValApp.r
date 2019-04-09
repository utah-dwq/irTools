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
runSiteValApp=function(master_site_file,polygon_path,edit_log_path,reasons_flat_file){
	 shiny::runApp(system.file('siteValApp', package='irTools'))
	 }
