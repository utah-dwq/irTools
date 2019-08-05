# site validation app ui
library(shiny)
library(shinyBS)
library(wqTools)
library(irTools)
library(leaflet)

site_type_choices=c("REVIEW","ACCEPT","REJECT", "FURTHER")
names(site_type_choices)=c("Review needed","Accepted","Rejected", "Further review")

options(warn = -1)

ui <-fluidPage(

# Header
headerPanel(
	title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
	tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Site Review Application")
),

mainPanel(width=11,
bsCollapse(id='collapse_panels', multiple=T, open=1,
	bsCollapsePanel(list(icon('file-import'),"Start"), value=1,
		fluidRow(textInput('reviewer', 'Reviewer')),
		fluidRow(
			column(2, fileInput("import_sites", "Import site file", accept=".xlsx")),
			#column(2, actionButton('demo_input', icon=icon('upload'), label='Use demo input', style = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4%")),
			column(2, actionButton('example_input', icon=icon('question'), label='', style = "margin-top: 25px;"))
		)		
	),
	bsCollapsePanel(list(icon('map-marked-alt'),"Review map"), value=2,
		fluidRow(
			column(2),
			column(3, shinyWidgets::pickerInput("site_types","Site types to map:", choices=site_type_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
			column(3, uiOutput("review_reasons")),
			column(3, uiOutput('ml_types')),
			column(1, shinyWidgets::materialSwitch(inputId = "auto_zoom", label="Auto-zoom on", value = TRUE, right=T, status='primary'))
		),

		# Map
		fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))
	),
	
	bsCollapsePanel(list(icon('edit'),"Review selected features"), value=3,
		# Reviewer actions
		fluidRow(
			#
			actionButton('clear_sel', 'Clear table selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('eraser')),
			actionButton('clear_all', 'Clear all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
			actionButton('accept', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
			actionButton('reject', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('minus-circle')),
			actionButton('add_reject_reason', 'Add rejection reason', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('plus-circle')),
			actionButton('merge', 'Merge', style='color: #fff; background-color: #337ab7; border-color: #2e6da4', icon=icon('object-group')),
			actionButton('flag_further', 'Flag for further review',style='color: #fff; background-color: #337ab7; border-color: #2e6da4', icon=icon('flag')),
			actionButton('reset', 'Reset sites to input',style='color: #fff; background-color: #337ab7; border-color: #2e6da4', icon=icon('undo')),
			downloadButton('exp_rev', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')
		),
	
		# Table
		fluidRow(div(DT::DTOutput("selected_sites_table"), style = "font-size:70%")),
		br(),
		br(),
		br()
	)

)
)
)
