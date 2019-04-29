# site validation app ui
library(shinyBS)

site_type_choices=c("REVIEW","ACCEPT","REJECT")
names(site_type_choices)=c("Review needed","Accepted","Rejected")



ui <-fluidPage(

# Header
headerPanel(
	title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
	tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Site Review Application")
),

mainPanel(width=10,
bsCollapse(multiple=T, open="Import sites file",
	bsCollapsePanel("Import sites file",
		fileInput("import_sites", "Import site file", accept=".xlsx")
	),
	bsCollapsePanel("Review map",
		fluidRow(
			column(1),
			column(2, shinyWidgets::pickerInput("site_types","Site types to map:", choices=site_type_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
			column(3, uiOutput("review_reasons")),
			column(3, uiOutput('ml_types')),
			column(2, shinyWidgets::materialSwitch(inputId = "auto_zoom", label="Auto-zoom on", value = TRUE, right=T, status='primary'))
		),

		# Map
		fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))
	),
	
	bsCollapsePanel("Edit selected features",
		# Reviewer actions
		fluidRow(
			actionButton('clear_sel', 'Clear table selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
			actionButton('clear_all', 'Clear all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
			actionButton('accept', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
			actionButton('reject', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
			actionButton('add_reject_reason', 'Add rejection reason', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
			actionButton('merge', 'Merge', style='color: #fff; background-color: #337ab7; border-color: #2e6da4'),
			actionButton('flag_further', 'Comment & flag for further review',style='color: #fff; background-color: #337ab7; border-color: #2e6da4')
		),
	
		# Table
		fluidRow(div(DT::DTOutput("selected_sites_table"), style = "font-size:70%")),
		br(),
		br(),
		br()
	),
	bsCollapsePanel("Export reviews",
		downloadButton('exp_rev', label = "Export reviews")
	)

)
)

)
	
