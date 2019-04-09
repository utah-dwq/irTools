# site validation app ui
library(shinyBS)

ui <-fluidPage(

# Header
headerPanel(
	title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
	tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Site Review Application")
),

mainPanel(width=10,
bsCollapse(multiple=T, open="Import files",
	bsCollapsePanel("Import files",
		fileInput("import_sites", "Import site file", accept=".csv"),
		fileInput("import_reasons", "Import review reasons file", accept=".csv")
	),
	bsCollapsePanel("Review map",
		fluidRow(
			column(2, checkboxGroupInput("site_types","Site types to map:", choiceNames=c("Review needed","Accepted","Rejected"), choiceValues=c("REVIEW","ACCEPT","REJECT"))),
			column(5, uiOutput("review_reasons")),
			column(3, uiOutput('ml_types')),
			column(2, checkboxInput('auto_zoom', "Auto-zoom to sites", value=TRUE))
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
	
