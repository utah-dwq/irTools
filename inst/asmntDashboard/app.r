### Assessment Dashboard
### Version 2

# Packages
library(wqTools)
library(irTools)
library(leaflet)
library(shinyBS)
library(plotly)
library(sf)
library(rgdal)

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard')
#site_use_param_asmnt=read.csv('data/site-use-param-asmnt.csv')

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

	mainPanel(width=12,
		bsCollapse(multiple=T,
			bsCollapsePanel(list(icon('plus-circle'), icon('file-import'),"Import assessments file"), 
				#fluidRow(
					column(2, fileInput("import_assessments", "Import assessment file", accept=".csv"),
					uiOutput('ex_url'))
					#column(2, actionButton('example_input', icon=icon('question'), label='', style = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4%"))
				#)
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"),
				fluidRow(
					column(3, fluidRow(
						actionButton('clear_au', 'Clear selected AU(s)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('trash-alt')),
					 	actionButton('build_tools', 'Build analysis tools', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('toolbox'))
					)),
					column(2, shinyWidgets::materialSwitch(inputId = "au_hover", label="Show AU assessments on hover", value = TRUE, right=T, status='primary'))
					#column(1),
					#column(3, shinyWidgets::pickerInput("site_types","Site types to map:", choices=site_type_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
					#column(3, uiOutput("review_reasons")),
					#column(3, uiOutput('ml_types')),
					
				),
				br(),
				
				# Map
				fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px"),size=2, color="#0080b7"))
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
			)
		)
	)
)

# Server
server <- function(input, output, session){

options(warn=0)

# Example input url
output$ex_url <-renderUI(a(href='https://github.com/utah-dwq/asmntDashboard/blob/version2/data/site-use-param-asmnt.csv',list(icon('question'),"Example input data"),target="_blank"))

# Empty reactive objects
reactive_objects=reactiveValues()

# Import site-use-param-assessments file
observeEvent(input$import_assessments,{
	file=input$import_assessments$datapath
	site_use_param_asmnt=read.csv(file)
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
	reactive_objects$rejected_sites=inputs$rejected_sites
	reactive_objects$na_sites=inputs$na_sites
	reactive_objects$master_site=inputs$master_site
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
	asmntMap(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt, reactive_objects$na_sites, reactive_objects$rejected_sites)
})
asmnt_map_proxy=leafletProxy('assessment_map')

# Map polygon click to select AUs
observeEvent(input$assessment_map_shape_click,{
	au_click = input$assessment_map_shape_click$id
	if(!is.null(au_click)){
		if(au_click %in% reactive_objects$selected_aus){
			reactive_objects$selected_aus=reactive_objects$selected_aus[!reactive_objects$selected_aus %in% au_click]
		}else{
			reactive_objects$selected_aus=append(reactive_objects$selected_aus, au_click)
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
	reactive_objects$sel_sites=sel_sites
	reactive_objects$sel_data=subset(merged_data, IR_MLID %in% sel_sites)
	reactive_objects$sel_crit=subset(criteria, IR_MLID %in% sel_sites)
	showModal(modalDialog(title="Analysis tools ready.",size="l",easyClose=T,
		"Data and analysis tools ready. Scroll to 'Figures' and 'Data table' panels to review and plot data."))
	sel_data<<-reactive_objects$sel_data
	sel_crit<<-reactive_objects$sel_crit
	
})


# Figures
observe({
	req(reactive_objects$sel_data, reactive_objects$sel_crit)
	figures=callModule(module=figuresMod, id='figures', sel_data=reactive_objects$sel_data, sel_crit=reactive_objects$sel_crit)
})

# Data table output
output$dt=DT::renderDT({
	req(reactive_objects$sel_data)
	DT::datatable(data.frame(lapply(reactive_objects$sel_data, as.factor)),
		selection='none', rownames=FALSE, filter="top",
		options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
	)
})

# Export data table
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


}

## run app
shinyApp(ui = ui, server = server)




