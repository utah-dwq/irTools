### Assessment Dashboard
### Version 2

#setwd('C:\\Users\\jvander\\Documents\\R\\irTools\\inst\\asmntDashboard')

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
source('modules/initialDataProc.R')
source('modules/asmntMap.R')
source('modules/figuresMod.R')

# Load data & criteria
load('data/prepped_merged_data.Rdata')

options(warn = -1)

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
	column(8, shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1, 
		bsCollapsePanel(list(icon('plus-circle'), icon('file-import'),"Import assessments file"), value=1, 
			#fluidRow(
				column(4, fileInput("import_assessments", "Import assessment file", accept=".xlsx"),
				uiOutput('ex_url')),
				column(2, actionButton('demo_input', icon=icon('upload'), label='Use demo input', style = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4%"))
			#)
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"),
			# Map
			shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px"),size=2, color="#0080b7")
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('chart-bar'), "Figures"),
			figuresModUI('figures')
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('table'), "Data table"),
			fluidRow(downloadButton('exp_dt', label = "Export data table", icon='download', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%')),
			br(),
			fluidRow(div(DT::DTOutput("dt"), style = list("font-size:65%")))
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('database'), "Download raw data from WQP"), 
			fluidRow(
				column(2, h4('Start date'), dateInput('start_date', '', format='mm/dd/yyyy', value='10/01/2008')),
				column(2, h4('End date'), dateInput('end_date', '', format='mm/dd/yyyy'))
			),
			uiOutput('wqp_url')
			#actionButton('dwnld_wqp', 'Download WQP data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('download'))
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('download'), "Export reviews"), 
			downloadButton('exp_rev', label = "Export reviews")
		)
	))),
	
	#Reviewer toolbar (wide)
	column(4,fixedPanel(draggable=T, style="z-index:1000;",
		shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1,
			bsCollapsePanel(list(icon('plus-circle'), icon('toolbox'), 'Toolbar'), value=1,
				fluidRow(
					textInput('rev_name', 'Reviewer name'),
					actionButton('clear_au', 'Clear selected AU(s)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('trash-alt')),
					actionButton('build_tools', 'Build analysis tools', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('toolbox')),
					actionButton('asmnt_accept','Accept (inactive)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle'))
				),
				fluidRow(column(12, uiOutput('rebuild')))
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('flag'),"Flag assessment"),
				uiOutput('flagUI1'),
				uiOutput('flagUI2'),
				uiOutput('flagUI3'),
				uiOutput('flagUI4'),
				uiOutput('flagUI5'),
				uiOutput('flagUI6'),
				uiOutput('flagUI7')
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('draw-polygon'),"Split AU"),
				uiOutput('splitUI')
			)
		))
	)),
	br(),
	br(),
	br()
)

# Server
server <- function(input, output, session){

#options(warn=0)

# Import all available WQP sites
#wqp_sites=wqTools::readWQP(type="sites", statecode="US:49", siteType=c("Lake, Reservoir, Impoundment","Stream", "Spring"))

# Example input url
output$ex_url <-renderUI(a(href='https://raw.githubusercontent.com/utah-dwq/irTools/master/inst/extdata/site-use-param-asmnt.csv',list(icon('question'),"Example input data"),target="_blank"))

# Empty reactive objects
reactive_objects=reactiveValues()

# Demo data input
observeEvent(input$demo_input, {
	file=system.file("extdata", "site-use-param-asmnt.xlsx", package = "irTools")	
	au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_splits=au_splits
	site_use_param_asmnt=as.data.frame(readxl::read_excel(file, 'site-use-param-asmnt'))
	reactive_objects$site_use_param_asmnt=site_use_param_asmnt
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
	reactive_objects$rejected_sites=inputs$rejected_sites
	reactive_objects$na_sites=inputs$na_sites
	reactive_objects$master_site=inputs$master_site
	reactive_objects$rebuild=FALSE
	showModal(shinyjqui::draggableModalDialog(easyClose=T, 'Demo data uploaded.'))
})

# Import site-use-param-assessments file
observeEvent(input$import_assessments,{
	file=input$import_assessments$datapath
	#site_use_param_asmnt=as.data.frame(readxl::read_excel(file, 'site-use-param-asmnt.xlsx'))
	au_splits=as.data.frame(readxl::read_excel(file, 'au-splits'))
	reactive_objects$au_splits=au_splits
	site_use_param_asmnt=as.data.frame(readxl::read_excel(file, 'site-use-param-asmnt'))
	#site_use_param_asmnt=read.csv("C:\\Users\\jvander\\Desktop\\site-use-param-asmnt - Copy.csv")
	reactive_objects$site_use_param_asmnt=site_use_param_asmnt
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
	reactive_objects$rejected_sites=inputs$rejected_sites
	reactive_objects$na_sites=inputs$na_sites
	reactive_objects$master_site=inputs$master_site
	reactive_objects$rebuild=FALSE
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
					'<br />', "ID w/ exceedance params: ", idE_params)
	})
})

# Map output
output$assessment_map=leaflet::renderLeaflet({
	req(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt)
	asmntMap(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites)#, wqp_sites=wqp_sites)
})
asmnt_map_proxy=leafletProxy('assessment_map')

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
observeEvent(input$au_hover, ignoreInit=T, {
	if(input$au_hover == FALSE){
		asmnt_map_proxy %>%
			clearGroup(group='Assessment units') %>%
			addPolygons(data=reactive_objects$au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"))
	}else{
		asmnt_map_proxy %>%
			clearGroup(group='Assessment units') %>%
			addPolygons(data=reactive_objects$au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
				label=lapply(reactive_objects$au_asmnt_poly$lab, HTML))
	}
})

# Highlight AU polygon by adding new polygons via proxy
observeEvent(reactive_objects$selected_aus, ignoreNULL = F, ignoreInit=T, {
	req(reactive_objects$au_asmnt_poly)
	asmnt_map_proxy %>%
	clearGroup(group='highlight') %>%
	addPolygons(data=reactive_objects$au_asmnt_poly[reactive_objects$au_asmnt_poly$ASSESS_ID %in% reactive_objects$selected_aus,],
		group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 10)
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
		reactive_objects$sel_data=subset(merged_data, IR_MLID %in% sel_sites)
		reactive_objects$sel_crit=subset(criteria, IR_MLID %in% sel_sites)
		showModal(modalDialog(title="Analysis tools ready.",size="l",easyClose=T,
			"Data and analysis tools ready. Scroll to 'Figures' and 'Data table' panels to review and plot data."))
	}else{
		showModal(modalDialog(title="No sites selected.",easyClose=T,
			"No assessed sites are associated with selected AUs."))
	}
})


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

# Export data table - (export wide dataset, not column subset dataset)
output$exp_dt <- downloadHandler(
	filename=paste0('exported-data-', Sys.Date(),'.xlsx'),
	content = function(file) {writexl::write_xlsx(
		list(data=reactive_objects$sel_data),
		path = file, format_headers=F, col_names=T)}
)

# Download WQP data for sites
observe({
	if(!is.null(reactive_objects$sel_sites)){
		siteids=reactive_objects$sel_sites
		orig_siteids=unique(reactive_objects$master_site[,c('IR_MLID','MonitoringLocationIdentifier')])
		orig_siteids=subset(orig_siteids, IR_MLID %in% siteids)
		orig_siteids=as.vector(unique(orig_siteids[,'MonitoringLocationIdentifier']))
		siteids=unique(append(as.character(siteids), orig_siteids))
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
output$build_to_proceed=renderText({"Select AU(s) and build analysis tools to proceed..."})

## AU split UI

output$splitUI=renderUI({
	if(is.null(reactive_objects$sel_sites)){
		textOutput('build_to_proceed')
	}else{
		tagList(
			actionButton('build_split_map','Build split map', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%',  icon=icon('map-marked-alt')),
			actionButton('split_cancel','Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('window-close')),
			actionButton('split_save','Save', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('save')),
			editModUI("auSplit"),
			helpText('Note - Map panning with mouse grab is disabled here. Use arrow keys to pan map.')
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
			sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, hover=F, options=leafletOptions(dragging=F)) %>%
				fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
				showGroup('Assessed sites') %>% clearControls()
			reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
		}
})


## Save splits
observeEvent(input$split_save, {
	req(reactive_objects$splits)
	splits=reactive_objects$splits()$finished
	if(!is.null(splits)){
		if(reactive_objects$drop_split=='Y' & dim(splits)[1]>=2){
			splits=splits[2:dim(splits)[1],]
		}
	splits=as.data.frame(splits)
	splits=splits[,!names(splits) %in% '_leaflet_id']
	splits$ASSESS_ID=reactive_objects$selected_aus[1]
	reactive_objects$au_splits=rbind(reactive_objects$au_splits, splits)
	print(reactive_objects$au_splits)
	au_asmnt_poly=subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)
	view=sf::st_bbox(au_asmnt_poly)
	site_asmnt=subset(reactive_objects$site_asmnt, IR_MLID %in% reactive_objects$sel_sites)
	sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, hover=F, options=leafletOptions(dragging=F)) %>%
		fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
		clearMarkers() %>% clearShapes() %>% clearControls()
	reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
	}
})

## Cancel splits
observeEvent(input$split_cancel, ignoreInit=T, {
	au_asmnt_poly=subset(reactive_objects$au_asmnt_poly, ASSESS_ID %in% reactive_objects$selected_aus)
	view=sf::st_bbox(au_asmnt_poly)
	site_asmnt=subset(reactive_objects$site_asmnt, IR_MLID %in% reactive_objects$sel_sites)
	sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites, hover=F, options=leafletOptions(dragging=F)) %>%
		fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
		clearMarkers() %>% clearShapes() %>% clearControls()
	reactive_objects$splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
})


# Export reviews
output$exp_rev <- downloadHandler(
	filename=paste0('asmnt-reviews-', Sys.Date(),'.xlsx'),
	content = function(file) {writexl::write_xlsx(
		list(asmnt_reviews=reactive_objects$site_use_param_asmnt),
		path = file, format_headers=F, col_names=T)}
)


# Print figures module outputs
observe({
	if(!is.null(figures$select_data())){
		print(figures$select_data())
	}
	if(!is.null(figures$param1())){
		print(figures$param1())
	}
	if(!is.null(figures$param_choices())){
		print(figures$param_choices())
	}

})


# Flag UI
param1=reactive({figures$param1()})
param_choices=reactive({figures$param_choices()})
ml_types_all=reactive({
	unique(reactive_objects$site_asmnt$MonitoringLocationTypeName)
})
ml_types_sel_au=reactive({
	unique(reactive_objects$site_asmnt$MonitoringLocationTypeName[reactive_objects$site_asmnt$IR_MLID %in% reactive_objects$sel_sites])
})
output$build_to_proceed2=renderText({"Select AU(s) and build analysis tools to proceed..."})

output$flagUI1=renderUI({
		if(is.null(reactive_objects$sel_sites)){
			textOutput('build_to_proceed2')
		}else{
			selectInput('flag_scope', 'Scope:', choices=c('Assessment unit(s)', 'Site(s)', 'Record(s)', 'State-wide'), selected=input$flag_scope)
		}
})

output$flagUI2=renderUI({
	req(ml_types_all())
	conditionalPanel(condition="input.flag_scope=='State-wide'",
		shinyWidgets::radioGroupButtons('flag_sw_ml_or_au', 'Apply flag by:', choices=c('AU type', 'ML type')),
		conditionalPanel(condition="input.flag_sw_ml_or_au == 'AU type'",
			shinyWidgets::pickerInput("flag_sw_au_type", "AU types:", choices=unique(reactive_objects$site_asmnt$AU_Type[order(reactive_objects$site_asmnt$AU_Type)]), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		),
		conditionalPanel(condition="input.flag_sw_ml_or_au == 'ML type'",
			shinyWidgets::pickerInput("flag_sw_ml_type", "ML types:", choices=ml_types_all(), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		)
	)
})

output$flagUI3=renderUI({
	req(reactive_objects$selected_aus, reactive_objects$sel_sites)
	conditionalPanel(condition="input.flag_scope=='Assessment unit(s)'",
		shinyWidgets::pickerInput("flag_aus", "Assessment unit(s):", choices=reactive_objects$selected_aus, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
	)
})

output$flagUI4=renderUI({
	req(reactive_objects$sel_sites, ml_types_sel_au())
	conditionalPanel(condition="input.flag_scope=='Site(s)' | input.flag_scope=='Record(s)'",
		shinyWidgets::radioGroupButtons('site_flag_type','Select sites by:', choices=c('MLID','ML type')),
		conditionalPanel(condition="input.site_flag_type=='MLID'",
			shinyWidgets::pickerInput("flag_sites", "Site(s):", choices=reactive_objects$sel_sites, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
		),
		conditionalPanel(condition="input.site_flag_type=='ML type'",
			shinyWidgets::pickerInput("flag_ml_types_sel_au", "ML types", choices=ml_types_sel_au(), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
		)
	)
})

output$flagUI5=renderUI({
	req(param_choices())
	shinyWidgets::pickerInput("flag_param", "Parameter(s):", choices=param_choices(), multiple=T, selected=param1(), options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
})


start_date=reactive({
	min(figures$select_data()$x)
})
end_date=reactive({
	max(figures$select_data()$x)
})

output$flagUI6=renderUI({
	req(input$flag_scope)
	conditionalPanel(condition="input.flag_scope=='Record(s)'",
		dateRangeInput('flag_date_range', 'Date range:', start=start_date(), end=end_date())
	)
})

output$flagUI7=renderUI({
	req(input$flag_scope)
	tagList(
		textInput('flag_comment', 'Comment:', placeholder='Enter comment...'),
		actionButton('flag_apply','Apply flag (inactive)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('flag'))
	)
})

}

## run app
shinyApp(ui = ui, server = server)

