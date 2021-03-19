### Assessment Dashboard
### Version 2

#setwd('C:\\Users\\jvander\\Documents\\R\\irTools\\inst\\asmntDashboard')
#devtools::install_local('C:\\Users\\jvander\\Documents\\R\\irTools', force=T)

# Packages
library(wqTools)
library(irTools)
library(leaflet)
library(shinyBS)
library(plotly)
library(sf)
library(rgdal)
library(mapedit)


# Modules/functions
source('helpers/initialDataProc.R')
source('helpers/asmntMap.R')
source('helpers/figuresMod.R')

# Load data & criteria
load(system.file("extdata", "asmntDashboard_data.Rdata", package = "irTools"))
load(system.file("extdata","reviewer_export_data.Rdata", package = "irTools"))
#load('C:\\Users\\jvander\\Documents\\R\\irTools\\inst\\extdata\\asmntDashboard_data.Rdata')
options(warn = -1)

# Shiny file input size allowed
options(shiny.maxRequestSize = 10*1024^2)

# User interface
ui <-fluidPage(

	# Header
	#headerPanel(
	#	title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
	#	tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="WQ Assessment Dashboard")
	#),
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="WQ Assessment Dashboard")
	),


	# User inputs & figures
	fluidRow(column(12, align='left', offset=8,
		actionButton('toolbar_reset', 'Reset toolbar', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('sync-alt'))
	)),
	br(),
	column(8, shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1,
		bsCollapsePanel(list(icon('plus-circle'), icon('file-import'),"Import assessments file"), value=1,
			#fluidRow(
				column(4, fileInput("import_assessments", "Import assessment file", accept=".xlsx"))#,
				#uiOutput('ex_url')),
				#column(2, actionButton('demo_input', icon=icon('upload'), label='Use demo input', style = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4%"))
			#)
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"), value=2,
			# Map
			uiOutput('map_rev_filter'),
			shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px", width="100%"),size=2, color="#0080b7")
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('chart-bar'), "Figures"), value=3,
			fluidRow(tableOutput("asmnt_summary")),
			figuresModUI('figures')
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('table'), "View & download data"), value=4,
			fluidRow(downloadButton('exp_dt', label = "Download data & assessment summary", icon=icon('download'), style='color: #fff; background-color: #337ab7; border-color: #2e6da4%')),
			br(),
			fluidRow(div(DT::DTOutput("dt"), style = list("font-size:65%")))
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('database'), "Download raw data from WQP"), value=5,
			fluidRow(
				column(2, h4('Start date'), dateInput('start_date', '', format='mm/dd/yyyy', value='10/01/2010')),
				column(2, h4('End date'), dateInput('end_date', '', format='mm/dd/yyyy', value='09/30/2018'))
			),
			uiOutput('wqp_url')
			#actionButton('dwnld_wqp', 'Download WQP data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('download'))
		)#,
		#bsCollapsePanel(list(icon('plus-circle'), icon('download'), "Export reviews"),
		#	downloadButton('exp_rev', label = "Export reviews")
		#)
	))),

	#Reviewer toolbar (wide)
	uiOutput('toolbarUI')
)

# Server
server <- function(input, output, session){


# Toolbar UI
#output$build_to_proceed3=renderText({"Select AU(s) and build analysis tools to proceed..."})
observeEvent(input$toolbar_reset, ignoreInit=F, ignoreNULL=F, {
	options(warn=-1)
	output$toolbarUI=renderUI({
		column(4,fixedPanel(draggable=T, style="z-index:1000; overflow-y:scroll; max-height: 85vh",
			shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1,
				bsCollapsePanel(list(icon('plus-circle'), icon('toolbox'), 'Toolbar'), value=1,
					fluidRow(
						textInput('rev_name', 'Reviewer name'),
						actionButton('clear_au', 'Clear selected AU(s)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('trash-alt')),
						actionButton('build_tools', 'Build analysis tools', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('toolbox')),
						downloadButton('exp_rev', label = "Export reviews", style='color: #fff; background-color: #337ab7; border-color: #2e6da4%')
						#actionButton('asmnt_accept','Accept (inactive)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle'))
					),
					fluidRow(column(12, uiOutput('rebuild')))
				),
				bsCollapsePanel(list(icon('plus-circle'), icon('edit'),"Review assessments"), value=2,
					uiOutput('flagUI1a'),
					uiOutput('flagUI1b'),
					uiOutput('flagUI2'),
					uiOutput('flagUI3'),
					uiOutput('flagUI4'),
					uiOutput('flagUI5'),
					uiOutput('flagUI6'),
					uiOutput('flagUI7'),
					uiOutput('flagUI8')
				),
				bsCollapsePanel(list(icon('plus-circle'), icon('draw-polygon'),"Split AU"), value=3,
					uiOutput('splitUI')
				)
			))
		))
	})
})

options(warn=0)

# Import all available WQP sites
#wqp_sites=wqTools::readWQP(type="sites", statecode="US:49", siteType=c("Lake, Reservoir, Impoundment","Stream", "Spring"))

# Example input url
#output$ex_url <-renderUI(a(href='https://raw.githubusercontent.com/utah-dwq/irTools/master/inst/extdata/site-use-param-asmnt.xlsx',list(icon('question'),"Example input data"),target="_blank"))

# Empty reactive objects
reactive_objects=reactiveValues()

# Demo data input
observeEvent(input$demo_input, {
	file=system.file("extdata", "asmntDashboard_demo_data.xlsx", package = "irTools")
	au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_splits=au_splits
	site_use_param_asmnt=as.data.frame(readxl::read_excel(file, 'site-use-param-asmnt'))
	site_use_param_asmnt$AU_review=as.character(site_use_param_asmnt$AU_review)
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_reviews=as.data.frame(readxl::read_excel(file, 'au-reviews'))
	reactive_objects$site_reviews=as.data.frame(readxl::read_excel(file, 'site-reviews'))
	reactive_objects$record_reviews=as.data.frame(readxl::read_excel(file, 'record-reviews'))
	reactive_objects$sw_reviews=as.data.frame(readxl::read_excel(file, 'sw-reviews'))
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	na_polys=st_set_geometry(reactive_objects$au_asmnt_poly[is.na(reactive_objects$au_asmnt_poly$AssessCat),colnames(reactive_objects$au_asmnt_poly) %in% colnames(site_use_param_asmnt)], NULL)
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
	reactive_objects$rejected_sites=inputs$rejected_sites
	reactive_objects$na_sites=inputs$na_sites
	reactive_objects$master_site=inputs$master_site
	reactive_objects$rebuild=FALSE
	site_use_param_asmnt=plyr::rbind.fill(site_use_param_asmnt, na_polys)
	site_use_param_asmnt$AU_review[is.na(site_use_param_asmnt$AU_review)] = 'Not assessed'
	reactive_objects$site_use_param_asmnt=site_use_param_asmnt
	showModal(shinyjqui::draggableModalDialog(easyClose=T, 'Demo data uploaded.'))
})

# Import site-use-param-assessments file
observeEvent(input$import_assessments,{
	file=input$import_assessments$datapath
	au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_splits=au_splits
	site_use_param_asmnt=as.data.frame(readxl::read_excel(file, 'site-use-param-asmnt'))
	site_use_param_asmnt$AU_review=as.character(site_use_param_asmnt$AU_review)
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_reviews=as.data.frame(readxl::read_excel(file, 'au-reviews'))
	reactive_objects$site_reviews=as.data.frame(readxl::read_excel(file, 'site-reviews'))
	reactive_objects$record_reviews=as.data.frame(readxl::read_excel(file, 'record-reviews'))
	reactive_objects$sw_reviews=as.data.frame(readxl::read_excel(file, 'sw-reviews'))
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	na_polys=st_set_geometry(reactive_objects$au_asmnt_poly[is.na(reactive_objects$au_asmnt_poly$AssessCat),colnames(reactive_objects$au_asmnt_poly) %in% colnames(site_use_param_asmnt)], NULL)
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
	reactive_objects$rejected_sites=inputs$rejected_sites
	reactive_objects$na_sites=inputs$na_sites
	reactive_objects$master_site=inputs$master_site
	reactive_objects$rebuild=FALSE
	site_use_param_asmnt=plyr::rbind.fill(site_use_param_asmnt, na_polys)
	site_use_param_asmnt$AU_review[is.na(site_use_param_asmnt$AU_review)] = 'Not assessed'
	reactive_objects$site_use_param_asmnt=site_use_param_asmnt
})




# Add html label to au_asmnt_poly
observe({
	req(reactive_objects$au_asmnt_poly)
	reactive_objects$au_asmnt_poly=within(reactive_objects$au_asmnt_poly, {
		lab=paste0(
					'<p>',
					"AU name: ", AU_NAME,
					'<br />', "AU ID: ", ASSESS_ID,
					'<br />', "Assessment: ", AssessCat,
					'<br />', "Impaired params: ", Impaired_params,
					'<br />', "ID w/ exceedance params: ", IDEX_params)
	})
})

# Map output
output$assessment_map=leaflet::renderLeaflet({
	req(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt)
	asmntMap(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites)#, wqp_sites=wqp_sites)
})
asmnt_map_proxy=leafletProxy('assessment_map')

# Map filter UI
output$map_rev_filter=renderUI({
	req(reactive_objects$site_use_param_asmnt)
	#req(reactive_objects$map_ready)
	choices=unique(reactive_objects$site_use_param_asmnt$AU_review)
	choices=unique(append(choices, c('Complete', 'Complete with flag(s)', 'Not assessed')))
	fluidRow(
		column(1),
		column(11,shinyWidgets::pickerInput('map_rev_filter', 'Review types', choices, selected=c('New listing', 'New listing, permit'), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")))
	)
})

# Filter map by review type
observeEvent(input$map_rev_filter, ignoreInit=T, {
	req(reactive_objects$site_use_param_asmnt)
	aus=unique(reactive_objects$site_use_param_asmnt$ASSESS_ID[reactive_objects$site_use_param_asmnt$AU_review %in% input$map_rev_filter])
	au_asmnt_poly=reactive_objects$au_asmnt_poly[reactive_objects$au_asmnt_poly$ASSESS_ID %in% aus,]
	au_asmnt_poly=within(au_asmnt_poly, {
	lab=paste0(
				'<p>',
				"AU name: ", AU_NAME,
				'<br />', "AU ID: ", ASSESS_ID,
				'<br />', "Assessment: ", AssessCat,
				'<br />', "Impaired params: ", Impaired_params,
				'<br />', "ID w/ exceedance params: ", IDEX_params,
				'<br> NS pollution indicators: ', pi_params)

	})

	if(dim(au_asmnt_poly)[1]>0){
		view=sf::st_bbox(au_asmnt_poly)
		asmnt_map_proxy %>%
			clearGroup(group='Assessment units') %>%
			addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~polyID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
						label=lapply(au_asmnt_poly$lab, HTML)) %>%
			fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))
	}else{
		asmnt_map_proxy %>%
			clearGroup(group='Assessment units')
	}
})


# Map polygon click to select AUs
observeEvent(input$assessment_map_shape_click,{
	au_click = input$assessment_map_shape_click$id
	if(!is.null(au_click)){
		au_id=as.character(unique(au_poly$ASSESS_ID[au_poly$polyID==au_click]))
		if(au_id %in% reactive_objects$selected_aus){
			reactive_objects$selected_aus=reactive_objects$selected_aus[!reactive_objects$selected_aus %in% au_id]
		}else{
			reactive_objects$selected_aus=append(reactive_objects$selected_aus, au_id)
		}
	}
})


# Turn off AU assessment hover info w/ switch
#observeEvent(input$au_hover, ignoreInit=T, {
#	if(input$au_hover == FALSE){
#		asmnt_map_proxy %>%
#			clearGroup(group='Assessment units') %>%
#			addPolygons(data=reactive_objects$au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"))
#	}else{
#		asmnt_map_proxy %>%
#			clearGroup(group='Assessment units') %>%
#			addPolygons(data=reactive_objects$au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
#				label=lapply(reactive_objects$au_asmnt_poly$lab, HTML))
#	}
#})

# Highlight AU polygon by adding new polygons via proxy
observeEvent(reactive_objects$selected_aus, ignoreNULL = F, ignoreInit=T, {
	req(reactive_objects$au_asmnt_poly)
	asmnt_map_proxy %>%
	clearGroup(group='highlight') %>%
	addPolygons(data=reactive_objects$au_asmnt_poly[reactive_objects$au_asmnt_poly$ASSESS_ID %in% reactive_objects$selected_aus,],
		group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
})

# Clear selected AUs with clear_au action button
observeEvent(input$clear_au, {
	reactive_objects$selected_aus=NULL
})

# Generate data and criteria subsets (based on selected AUs) for analysis tools on button press
observeEvent(input$build_tools,{
	sel_sites=reactive_objects$site_asmnt$IR_MLID[reactive_objects$site_asmnt$ASSESS_ID %in% reactive_objects$selected_aus]
	if(length(sel_sites)>0){
		reactive_objects$sel_sites=sel_sites
		reactive_objects$sel_data=subset(assessed_data, IR_MLID %in% sel_sites)
		reactive_objects$sel_crit=subset(criteria, IR_MLID %in% sel_sites)
		reactive_objects$asmnt_summary=unique(sf::st_drop_geometry(subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)))
		showModal(modalDialog(title="Analysis tools ready.",size="l",easyClose=T,
			"Data and analysis tools ready. Scroll to 'Figures' and 'Data table' panels to review and plot data."))
	}else{
		showModal(modalDialog(title="No sites selected.",easyClose=T,
			"No assessed sites are associated with selected AUs."))
	}
})


output$asmnt_summary=function() {
	req(reactive_objects$asmnt_summary)
	knitr::kable(allign='c', reactive_objects$asmnt_summary[,c('ASSESS_ID','AU_NAME','AssessCat','Impaired_params','IDEX_params','pi_params')],
			row.names=F, col.names=c('ASSESS_ID','AU_NAME','Category', 'Impaired params', 'ID w/ exceedance params', 'NS pollution indicators')) %>%
			kableExtra::kable_styling()
}

# Recommend rebuild
## Determine if a rebuild is appropriate
observe({
	req(reactive_objects$sel_data)
	data_aus=(unique(reactive_objects$sel_data$ASSESS_ID))[order(unique(reactive_objects$sel_data$ASSESS_ID))]
	if(length(reactive_objects$selected_aus)>0){
		map_aus=reactive_objects$selected_aus[order(reactive_objects$selected_aus)]
		reactive_objects$rebuild=!all(data_aus==map_aus)
	}else{reactive_objects$rebuild=TRUE}
	#print(reactive_objects$rebuild)
})

## Generate rebuild UI (see https://stackoverflow.com/questions/37470226/blinking-loading-text-in-r-shiny)
output$rebuild=renderUI({
	req(reactive_objects$rebuild)
	if(reactive_objects$rebuild){
		fluidRow(
			tags$div("Re-build recommended...",id="rebuild_message"),
			tags$script(HTML("
				(function blink() {
					$('#rebuild_message').fadeOut(500).fadeIn(500, blink);
				})();
			"))
		)
	}
})


# Figures
sel_data=reactive(reactive_objects$sel_data)
sel_crit=reactive(reactive_objects$sel_crit)

figures=callModule(module=figuresMod, id='figures', sel_data, sel_crit)

# Data table output
output$dt=DT::renderDT({
	req(reactive_objects$sel_data)
	DT::datatable(data.frame(lapply(reactive_objects$sel_data, as.factor)),
		selection='none', rownames=FALSE, filter="top",
		options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
	)
})

## Create styles for export headers
Identifier = openxlsx::createStyle(textDecoration = "bold", bgFill = "yellow")
IR = openxlsx::createStyle(textDecoration = "bold", bgFill = "pink")
Param = openxlsx::createStyle(textDecoration = "bold", bgFill = "turquoise")

observe({
 req(reactive_objects$sel_data)
  
  reviewer_export <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(reviewer_export, sheetName = "Data Summary")
  openxlsx::addWorksheet(reviewer_export, sheetName = "Abbreviated Data")
 # Narrow compiled data to clicked AU's
 compiled_data_narrow = lapply(compiled_data, function(x){x = x[x$ASSESS_ID%in%unique(reactive_objects$sel_data$ASSESS_ID),]})

 openxlsx::writeDataTable(reviewer_export, sheet = "Abbreviated Data", compiled_data_narrow$toxconv_data_asmnt)
 openxlsx::writeDataTable(reviewer_export, sheet = "Data Summary", compiled_data_narrow$summary_tc_assessed)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Abbreviated Data", cols = 1:95, rows = 1, rule = "IR", type = "contains", style = IR)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Abbreviated Data", cols = 1:95, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Abbreviated Data", cols = 1:95, rows = 1, rule = "Param", type = "contains",style = Param)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Data Summary", cols = 1:20, rows = 1, rule = "IR", type = "contains", style = IR)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Data Summary", cols = 1:20, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
 openxlsx::conditionalFormatting(reviewer_export, sheet = "Data Summary", cols = 1:20, rows = 1, rule = "Param", type = "contains",style = Param)


 if(!is.null(compiled_data_narrow$ecoli_data_asmnt)){
   openxlsx::addWorksheet(reviewer_export, sheetName = "E.coli Data")
   openxlsx::writeData(reviewer_export, sheet = "E.coli Data", compiled_data_narrow$ecoli_data_asmnt)
 }

 reactive_objects$AUexport = reviewer_export
})

# Export data table - (export wide dataset, not column subset dataset)
output$exp_dt <- downloadHandler(
filename=paste0('AU_data_export-', reactive_objects$sel_data$ASSESS_ID[1],"-",Sys.Date(),'.xlsx'),
content = function(file) {openxlsx::saveWorkbook(reactive_objects$AUexport, file)}
)

# Download WQP data for sites
observe({
	if(!is.null(reactive_objects$sel_sites)){
		siteids=reactive_objects$sel_sites
		orig_siteids=unique(reactive_objects$master_site[reactive_objects$master_site$not_in_wqp=='N',c('IR_MLID','MonitoringLocationIdentifier')])
		orig_siteids=subset(orig_siteids, IR_MLID %in% siteids)
		orig_siteids=as.vector(unique(orig_siteids[,'MonitoringLocationIdentifier']))
		siteids=na.omit(unique(append(as.character(siteids), orig_siteids)))
		#print(siteids)
		wqp_url=wqTools::readWQP(start_date=input$start_date, end_date=input$end_date, type='result', siteid=siteids, url_only=T)
		wqp_url=gsub("\\?", "#", wqp_url)
		wqp_url=gsub("https://www.waterqualitydata.us/data/Result/search", "https://www.waterqualitydata.us/portal/", wqp_url)
		wqp_url=gsub("&zip=no", "", wqp_url)
		reactive_objects$wqp_url=wqp_url
	}
})
output$wqp_url <-renderUI(a(href=paste0(reactive_objects$wqp_url),"Download WQP data",target="_blank"))


# AU splits
#output$build_to_proceed=renderText({"Select AU(s) and build analysis tools to proceed..."})

## AU split UI

output$splitUI=renderUI({
	if(is.null(reactive_objects$sel_sites)){
		#textOutput('build_to_proceed')
	}else{
		tagList(
			actionButton('build_split_map','Build split map', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%',  icon=icon('map-marked-alt')),
			editModUI("auSplit", height='450px'),
			helpText('Note - Map panning with mouse grab is disabled here. Use arrow keys to pan map.'),
			actionButton('split_cancel','Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('window-close')),
			actionButton('split_save','Save', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('save')),
			textInput('split_comment', 'Comment:', placeholder='Enter comment...')
		)
	}
})


## Build split map
observeEvent(input$build_split_map, {
	req(reactive_objects$selected_aus, reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites)
		if(length(reactive_objects$selected_aus)>1){
			showModal(modalDialog(easyClose=T, title = 'Select a single AU.', 'Splits can only be performed on one AU at a time. Please select a single AU before proceeding.'))
		}else{
			if(!is.null(reactive_objects$splits)){
				reactive_objects$drop_split='Y'
			}else{reactive_objects$drop_split='N'}
			au_asmnt_poly=subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)
			view=sf::st_bbox(au_asmnt_poly)
			site_asmnt=subset(reactive_objects$site_asmnt, IR_MLID %in% reactive_objects$sel_sites)
			sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, dragging=F) %>%#, options=leafletOptions(dragging=F)) %>%
				addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~polyID, weight=3,color=~col, options = pathOptions(pane = "au_poly")) %>%
				fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
				showGroup('Assessed sites') %>% clearControls()
			reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
		}
})


## Save splits
observeEvent(input$split_save, {
	req(reactive_objects$splits)
	splits=reactive_objects$splits()$finished
	if(input$rev_name!=''){
		if(!is.null(splits)){
			if(reactive_objects$drop_split=='Y' & dim(splits)[1]>=2){
				splits=splits[2:dim(splits)[1],]
			}
			splits=as.data.frame(splits)
			splits=splits[,!names(splits) %in% 'X_leaflet_id']
			splits$ASSESS_ID=reactive_objects$selected_aus[1]
			splits$Comment=input$split_comment
			splits$Reviewer=input$rev_name
			splits$geometry=st_as_text(splits$geometry)
			splits$ReviewDate=Sys.Date()
			reactive_objects$au_splits=rbind(reactive_objects$au_splits, splits)
			au_asmnt_poly=subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)
			view=sf::st_bbox(au_asmnt_poly)
			site_asmnt=subset(reactive_objects$site_asmnt, IR_MLID %in% reactive_objects$sel_sites)
			sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, dragging=F) %>%
				fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
				clearMarkers() %>% clearShapes() %>% clearControls()
			reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
			showModal(modalDialog(title='Split(s) saved.', easyClose=T))
		}else{showModal(modalDialog(title='Nothing drawn.', 'Finish drawing recommended splits before saving.', easyClose=T))}
	}else{showModal(modalDialog(title='Reviewer name required.', 'Fill in reviewer name above before saving.', easyClose=T))}
	print(splits)
})

## Cancel splits
observeEvent(input$split_cancel, ignoreInit=T, {
	au_asmnt_poly=subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)
	view=sf::st_bbox(au_asmnt_poly)
	site_asmnt=subset(reactive_objects$site_asmnt, IR_MLID %in% reactive_objects$sel_sites)
	sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, dragging=F) %>%
		fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
		clearMarkers() %>% clearShapes() %>% clearControls()
	reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
})


# Print figures module outputs
#observe({
#	if(!is.null(figures$select_data())){
#		print(figures$select_data())
#	}
#	if(!is.null(figures$param1())){
#		print(figures$param1())
#	}
#	if(!is.null(figures$param_choices())){
#		print(figures$param_choices())
#	}
#
#})


# Flag UI
param1=reactive({figures$param1()})
param_choices=reactive({figures$param_choices()})
ml_types_all=reactive({
	na.omit(unique(reactive_objects$site_asmnt$MonitoringLocationTypeName))
})
ml_types_sel_au=reactive({
	na.omit(unique(reactive_objects$site_asmnt$MonitoringLocationTypeName[reactive_objects$site_asmnt$IR_MLID %in% reactive_objects$sel_sites]))
})
#output$build_to_proceed2=renderText({"Select AU(s) and build analysis tools to proceed..."})

output$flagUI1a=renderUI({
		if(is.null(reactive_objects$sel_sites)){
			#textOutput('build_to_proceed2')
		}else{
			tagList(
				shinyWidgets::radioGroupButtons('rev_type', 'Review type:', choices=c('Generate flag','Mark complete'), checkIcon = list(yes = icon("check")))
			)
		}
})

output$flagUI1b=renderUI({
	req(input$rev_type)
	conditionalPanel(condition="input.rev_type=='Generate flag'",
		shinyWidgets::radioGroupButtons('flag_scope', 'Scope:', choices=c('Assessment unit(s)', 'Site(s)', 'Record(s)', 'State-wide'), selected=input$flag_scope, checkIcon = list(yes = icon("check")))
	)
})

output$flagUI2=renderUI({
	req(ml_types_all())
	conditionalPanel(condition="input.flag_scope=='State-wide' & input.rev_type=='Generate flag'",
		shinyWidgets::radioGroupButtons('flag_sw_ml_or_au', 'Apply flag by:', choices=c('AU type', 'ML type'), checkIcon = list(yes = icon("check"))),
		conditionalPanel(condition="input.flag_sw_ml_or_au == 'AU type'",
			shinyWidgets::pickerInput("flag_sw_au_type", "AU types:", choices=na.omit(unique(reactive_objects$site_asmnt$AU_Type[order(reactive_objects$site_asmnt$AU_Type)])), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		),
		conditionalPanel(condition="input.flag_sw_ml_or_au == 'ML type'",
			shinyWidgets::pickerInput("flag_sw_ml_type", "ML types:", choices=ml_types_all(), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		)
	)
})

output$flagUI3=renderUI({
	#req(reactive_objects$selected_aus, reactive_objects$sel_sites)
	conditionalPanel(condition="input.flag_scope=='Assessment unit(s)' | input.rev_type=='Mark complete'",
		shinyWidgets::pickerInput("flag_aus", "Assessment unit(s):", choices=reactive_objects$selected_aus, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
	)
})

output$flagUI4=renderUI({
	req(reactive_objects$sel_sites, ml_types_sel_au())
	conditionalPanel(condition="input.flag_scope=='Site(s)' & input.rev_type=='Generate flag'",
		shinyWidgets::radioGroupButtons('site_flag_type','Select sites by:', choices=c('MLID','ML type'), checkIcon = list(yes = icon("check"))),
		conditionalPanel(condition="input.site_flag_type=='MLID'",
			shinyWidgets::pickerInput("flag_sites", "Site(s):", choices=as.character(reactive_objects$sel_sites), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		),
		conditionalPanel(condition="input.site_flag_type=='ML type'",
			shinyWidgets::pickerInput("flag_ml_types_sel_au", "ML types", choices=ml_types_sel_au(), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		)
	)
})

output$flagUI5=renderUI({
	req(param_choices())
	conditionalPanel(condition="input.rev_type=='Generate flag' & input.flag_scope!='Record(s)'",
		shinyWidgets::pickerInput("flag_param", "Parameter(s):", choices=param_choices(), multiple=T, selected=param1(), options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
	)
})

observe({
	req(figures$select_data())
	reactive_objects$start_date=min(figures$select_data()$x)
	reactive_objects$end_date=max(figures$select_data()$x)
	ymin=min(figures$select_data()$y)
	ymax=max(figures$select_data()$y)
	selected_points=subset(reactive_objects$sel_data,
		R3172ParameterName==param1() &
		wqTools::facToNum(IR_Value) >= ymin &
		wqTools::facToNum(IR_Value) <= ymax &
		as.Date(ActivityStartDate) >= reactive_objects$start_date &
		as.Date(ActivityStartDate) <= reactive_objects$end_date
	)
	selected_rids=unique(selected_points[,c('ResultIdentifier','ActivityStartDate','IR_MLID','IR_MLNAME','ASSESS_ID','R3172ParameterName')])
	reactive_objects$selected_rids=selected_rids
})

output$flagUI6=renderUI({
	tagList(
		conditionalPanel(condition="input.rev_type=='Generate flag' & input.flag_scope=='Record(s)'",
		shinyWidgets::radioGroupButtons('record_flag_type','Select records by:', choices=c('Scatter plot','Record ID'), checkIcon = list(yes = icon("check"))),
			conditionalPanel(condition="input.record_flag_type=='Scatter plot'",
				helpText('Use the box select tool on the multi-site time series plot to select data to flag interactively.')
			),
			conditionalPanel(condition="input.record_flag_type=='Record ID'",
				shinyWidgets::pickerInput("flag_record_ids", "Record ID(s):", choices=as.character(unique(reactive_objects$sel_data$ResultIdentifier)), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
			)
		)
	)
})

output$flagUI7=renderUI({
	#req(input$flag_scope, input$flag_param)
	conditionalPanel(condition="input.rev_type=='Generate flag'",
		textInput('rev_comment', 'Comment:', placeholder='Enter comment...')
	)
})

output$flagUI8=renderUI({
	req(input$flag_scope)
	tagList(
		conditionalPanel(condition="input.rev_type=='Generate flag'",
			actionButton('flag_apply','Apply flag', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('flag'))
		),
		conditionalPanel(condition="input.rev_type=='Mark complete'",
			actionButton('mark_complete','Mark review complete', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle'))
		)
	)

})



observeEvent(input$mark_complete, ignoreInit=T, {
	# Mark reviews for selected AUs as complete w/ no flags or complete w/ flags in reactive_objects$site_use_param_asmnt
	if(input$rev_name!="" & !is.null(input$flag_aus)){
		flag_aus=input$flag_aus
		au_flags=unique(reactive_objects$au_reviews[reactive_objects$au_reviews$ASSESS_ID %in% flag_aus,]$ASSESS_ID)
		print(au_flags)
		site_flags=unique(reactive_objects$site_reviews[reactive_objects$site_reviews$ASSESS_ID %in% flag_aus,]$ASSESS_ID)
		print(site_flags)
		record_flags=unique(reactive_objects$record_reviews[reactive_objects$record_reviews$ASSESS_ID %in% flag_aus,]$ASSESS_ID)
		print(site_flags)
		flags=unique(c(as.character(au_flags), as.character(site_flags), as.character(record_flags)))
		complete_flag_aus=flag_aus[flag_aus %in% flags]
		complete_no_flag_aus=flag_aus[!flag_aus %in% flags]

		reactive_objects$site_use_param_asmnt=within(reactive_objects$site_use_param_asmnt, {
			AU_review[ASSESS_ID %in% complete_flag_aus] = 'Complete with flag(s)'
			AU_review[ASSESS_ID %in% complete_no_flag_aus] = 'Complete'
		})

		# Update AU polygons in map
		aus=unique(reactive_objects$site_use_param_asmnt$ASSESS_ID[reactive_objects$site_use_param_asmnt$AU_review %in% input$map_rev_filter])
		au_asmnt_poly=reactive_objects$au_asmnt_poly[reactive_objects$au_asmnt_poly$ASSESS_ID %in% aus,]
		au_asmnt_poly=within(au_asmnt_poly, {
		lab=paste0(
					'<p>',
					"AU name: ", AU_NAME,
					'<br />', "AU ID: ", ASSESS_ID,
					'<br />', "Assessment: ", AssessCat,
					'<br />', "Impaired params: ", Impaired_params,
					'<br />', "ID w/ exceedance params: ", IDEX_params)

		})

		if(dim(au_asmnt_poly)[1]>0){
			view=sf::st_bbox(au_asmnt_poly)
			asmnt_map_proxy %>%
				clearGroup(group='Assessment units') %>%
				addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~polyID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
							label=lapply(au_asmnt_poly$lab, HTML)) %>%
				fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4]))
		}else{
			asmnt_map_proxy %>%
				clearGroup(group='Assessment units')
		}
		# Clear completed AUs from selected AUs
		reactive_objects$selected_aus=reactive_objects$selected_aus[!reactive_objects$selected_aus %in% flag_aus]
		showModal(modalDialog(title='Reviews marked complete', 'Your reviews have been marked complete. Please remember to export your reviews to save.', easyClose=T))
	}else{showModal(modalDialog(title='Inputs needed', 'Finish filling out reviewer inputs before saving.', easyClose=T))}


})

# Save reviews
observeEvent(input$flag_apply, ignoreInit=T, {
	if(input$flag_scope=='Assessment unit(s)'){
		if(input$rev_name!="" & !is.null(input$flag_aus) & !is.null(input$flag_param) & input$rev_comment!=""){
			reviews=merge(input$flag_aus, input$flag_param)
			names(reviews)=c('ASSESS_ID','R3172ParameterName')
			reviews$Reviewer=input$rev_name
			reviews$Comment=input$rev_comment
			reviews$ReviewDate=Sys.Date()
			reactive_objects$au_reviews=rbind(reactive_objects$au_reviews, reviews)
			print(reactive_objects$au_reviews)
			showModal(modalDialog(title='Flags applied', 'Your flag has been applied. Continue reviewing selected AUs or mark as complete.', easyClose=T))
		}else{showModal(modalDialog(title='Inputs needed', 'Finish filling out reviewer inputs before saving.', easyClose=T))}
	}
	if(input$flag_scope=='Site(s)'){# | input$flag_scope=='Record(s)'){
		if((input$flag_scope=='Site(s)' &
			(input$rev_name!="" &
					((input$site_flag_type=='MLID' & !is.null(input$flag_sites)) | (input$site_flag_type=='ML type' & !is.null(input$flag_ml_types_sel_au))) &
					!is.null(input$flag_param) &  !is.null(input$rev_name) &  input$rev_comment!="")
			#|(input$flag_scope=='Record(s)' &
			#	(input$rev_name!="" &
			#		((input$site_flag_type=='MLID' & !is.null(input$flag_sites)) | (input$site_flag_type=='ML type' & !is.null(input$flag_ml_types_sel_au))) &
			#		!is.null(input$flag_param) &  !is.null(input$rev_name) &  input$rev_comment!="" & input$flag_date_range[1]!=Sys.Date()))
		)){
			if(input$site_flag_type=='MLID'){
				sites=input$flag_sites
			}else{
				sites=reactive_objects$site_asmnt$IR_MLID[reactive_objects$site_asmnt$IR_MLID %in% reactive_objects$sel_sites & reactive_objects$site_asmnt$MonitoringLocationTypeName %in% input$flag_ml_types_sel_au]
			}
			site_metadata=reactive_objects$site_asmnt[reactive_objects$site_asmnt$IR_MLID %in% sites, c('IR_MLID', 'IR_MLNAME', 'ASSESS_ID', 'MonitoringLocationTypeName'),]
			sites=data.frame(sites)
			names(sites)='IR_MLID'
			reviews=merge(sites, site_metadata)
			names(reviews)=c('IR_MLID', 'IR_MLNAME', 'ASSESS_ID','ML_Type')
			reviews=merge(reviews, input$flag_param)
			names(reviews)=c('IR_MLID', 'IR_MLNAME', 'ASSESS_ID','ML_Type', 'R3172ParameterName')
			reviews$Reviewer=input$rev_name
			reviews$Comment=input$rev_comment
			reviews$ReviewDate=Sys.Date()
			reactive_objects$site_reviews=rbind(reactive_objects$site_reviews, reviews)
			print(reactive_objects$site_reviews)
			showModal(modalDialog(title='Flags applied', 'Your flag has been applied. Continue reviewing selected AUs or mark as complete.', easyClose=T))

		}else{showModal(modalDialog(title='Inputs needed', 'Finish filling out reviewer inputs before saving.', easyClose=T))}
	}
	if(input$flag_scope=='Record(s)'){
		if(!is.null(input$rev_name) & input$rev_comment!="" & ((input$record_flag_type=='Scatter plot' & !is.null(reactive_objects$selected_rids))) | (input$record_flag_type=='Record ID' & !is.null(input$flag_record_ids))){
			if(input$record_flag_type=='Scatter plot'){
				reviews=reactive_objects$selected_rids
				reactive_objects$selected_rids=reactive_objects$selected_rids[0,]
			}else{
				reviews=unique(reactive_objects$sel_data[reactive_objects$sel_data$ResultIdentifier %in% input$flag_record_ids,c('ResultIdentifier','ActivityStartDate','IR_MLID','IR_MLNAME','ASSESS_ID','R3172ParameterName')])
			}
			reviews$Reviewer=input$rev_name
			reviews$Comment=input$rev_comment
			reviews$ReviewDate=Sys.Date()
			reactive_objects$record_reviews=unique(rbind(reactive_objects$record_reviews, reviews))
			print(reactive_objects$record_reviews)
			showModal(modalDialog(title='Flags applied', 'Your flag has been applied. Continue reviewing selected AUs or mark as complete.', easyClose=T))
		}else{showModal(modalDialog(title='Inputs needed', 'Finish filling out reviewer inputs before saving.', easyClose=T))}
	}
	if(input$flag_scope=='State-wide'){
		if(input$rev_name!="" & ((input$flag_sw_ml_or_au=='AU type' & !is.null(input$flag_sw_au_type)) | (input$flag_sw_ml_or_au=='ML type' & !is.null(input$flag_sw_ml_type))) & !is.null(input$flag_param)){
			if(input$flag_sw_ml_or_au=='AU type'){
				sw_reviews=merge(input$flag_sw_au_type, input$flag_param)
				sw_reviews$ML_Type=NA
				names(sw_reviews)=c('AU_Type','R3172ParameterName','ML_Type')
			}else{
				sw_reviews=merge(input$flag_sw_ml_type, input$flag_param)
				sw_reviews$AU_Type=NA
				names(sw_reviews)=c('ML_Type','R3172ParameterName','AU_Type')
			}
			sw_reviews$Reviewer=input$rev_name
			sw_reviews$Comment=input$rev_comment
			sw_reviews$ReviewDate=Sys.Date()
			print(reactive_objects$sw_reviews)
			reactive_objects$sw_reviews=rbind(reactive_objects$sw_reviews,sw_reviews)
			print(reactive_objects$sw_reviews)
			showModal(modalDialog(title='Flags applied', 'Your flag has been applied. Continue reviewing selected AUs or mark as complete.', easyClose=T))
		}else{showModal(modalDialog(title='Inputs needed', 'Finish filling out reviewer inputs before saving.', easyClose=T))}
	}
})


# Export reviews
output$exp_rev <- downloadHandler(
	filename=paste0('asmnt-reviews-', Sys.Date(),'.xlsx'),
	content = function(file) {writexl::write_xlsx(
		list(
			'site-use-param-asmnt'=subset(reactive_objects$site_use_param_asmnt, AU_review!='Not assessed'),
			'au-splits'=reactive_objects$au_splits,
			'au-reviews'=reactive_objects$au_reviews,
			'site-reviews'=reactive_objects$site_reviews,
			'record-reviews'=reactive_objects$record_reviews,
			'sw-reviews'=reactive_objects$sw_reviews
		),
		path = file, format_headers=F, col_names=T)}
)

}

## run app
shinyApp(ui = ui, server = server)
