#Delist Map to visualize NPS projects near delisting sites

# Packages
library(wqTools)
library(irTools)
library(leaflet)
library(shinyBS)
library(plotly)
library(sf)
library(rgdal)
library(mapedit)

# Step1 Create Map
# Basic default map with delist_mlids1 will be points on map along with AUsPolygon Shapes, and nps_proj points (these are described and loaded within the asmntMap())

#Step 2 When a user selects one of these AssessmentUnit polygons it will then load the data for that ASSESS_ID for the 
#two tables below delist_candidates1 AND nps_proj table below that for that ASSESS_ID.

# Step 3 For that selected AU there will be an option on the tool bar on the right to select one of the values from R3172ParameterName and add a comment for that parameter for the selected AU in the delist_candidates1 table. 
#On that same tool bar it will have an option to mark the ASSESS_ID as Complete which would  hide or remove the polygon shape for that AU from the map. To visualize the map easier and track what has been completed.

# Step 4 The tool bar will also have an export button to export the delist_candidates1 table with the comments added for the R3172ParameterName. 

# Modules/functions
source('helpers/asmntMap.R')

# Load delist_candidates1, delist_mlids, nps_proj
load(system.file("extdata", "delist_NPS_mlids.Rdata", package = "irTools")) #inlcudes df delist_mlids, delist_candidates1, nps_proj

options(warn = 0)

# Shiny file input size allowed
options(shiny.maxRequestSize = 10*1024^2)

# User interface
ui <-fluidPage(

	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="WQ Delistings Associated NPS Projects")
	),
	# User inputs & figures
	fluidRow(column(12, align='left', offset=8,
		actionButton('toolbar_reset', 'Reset toolbar', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('sync-alt'))
	)), 
	br(),
	column(8, shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1,
		bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"), value=2,
			shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px", width="100%"),size=2, color="#0080b7")
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('table'), "Parameter Delistings for AU"), value=4,
			fluidRow(div(DT::DTOutput("dt1"), style = list("font-size:65%")))
		),
		bsCollapsePanel(list(icon('plus-circle'), icon('table'), "NPS Projects in AU"), value=5,
		  fluidRow(div(DT::DTOutput("dt2"), style = list("font-size:65%"))),
			)
	))),

	#Reviewer toolbar (wide)
	uiOutput('toolbarUI')
)
  
server <- function(input, output, session) {
  # Assuming delist_candidates1, delist_mlids, nps_proj are loaded and available
  
  # Reactive value for selected ASSESS_ID
  selected_aus = reactiveVal()
  completed_assess_ids =reactiveVal()
  #filter for non reviews AUs
  delist_au_poly1 <- reactive({
    delist_au_poly[!delist_au_poly$ASSESS_ID %in% completed_assess_ids(), ]
  })
  
  # Initialize the map
  output$assessment_map=leaflet::renderLeaflet({
    req(delist_au_poly1())
    asmntMap(delist_au_poly1)
  })
  asmnt_map_proxy=leafletProxy('assessment_map')
  observe({
    # Update map based on delist_au_poly1 completed or not..
    leafletProxy("assessment_map", data = delist_au_poly1()) %>%
      clearLayers() %>%  # Clear specific layers if needed
      addPolygons(...)   # Add updated polygons
  })
  
  observeEvent(input$assessment_map_shape_click, {
    au_click = input$assessment_map_shape_click$id
    if (!is.null(au_click)) {
      # Assuming delist_au_poly1() is a reactive expression
      au_id = as.character(unique(delist_au_poly1()$ASSESS_ID[delist_au_poly1()$polyID == au_click]))
      current_selected_aus=selected_aus()
      if (au_id %in% current_selected_aus) {
       selected_aus(current_selected_aus[!current_selected_aus %in% au_id])
      } else {
        selected_aus(append(current_selected_aus, au_id))
      }}
  })
  
  observeEvent(selected_aus, ignoreNULL = F, ignoreInit=T, {
    req(delist_au_poly1)
    asmnt_map_proxy %>%
      clearGroup(group='highlight') %>%
      addPolygons(data=delist_au_poly1[delist_au_poly1$ASSESS_ID %in% selected_aus(),], group='highlight', 
            options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
  })
  
  # Render tables based on selected ASSESS_ID
  output$dt1 <- renderDT({
    req(selected_aus())
    # Filter delist_candidates1 based on selected ASSESS_ID
      DT::datatable(delist_candidates1[delist_candidates1$ASSESS_ID==selected_aus(),],
          selection='single', rownames=FALSE, filter="top",
          options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
      )
    })
  
  # Observer to update other components based on selected row
  observeEvent(input$dt1_rows_selected, {
    selected_row <- input$dt1_rows_selected
    if (length(selected_row) > 0) {
      # Get the R3172ParameterName from the selected row
      selected_param <- delist_candidates1[selected_row, ]$R3172ParameterName
      # Update the textInput for param_comment
      updateTextInput(session, "param_comment", value = paste("Comment for", selected_param))
    }
  })
  
  #Render the NPS Projects table
  output$dt2 <- renderDT({
    req(selected_assess_id())
    # Filter nps_proj based on selected ASSESS_ID
    DT::datatable(nps_proj[nps_proj$ASSESS_ID==selected_aus(),],
        selection='none', rownames=FALSE, filter="top",
        options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
    )
  })
  
  # Toolbar interaction for comments and marking complete
  observeEvent(input$submit_comment, {
    # Code to handle comment submission
    # Update delist_candidates1 with the comment
  })
  
  
  # Observe marking an ASSESS_ID as complete
  observeEvent(input$mark_complete, {
    # Add the completed ASSESS_ID to the list
    current_completed <- completed_assess_ids()
    completed_assess_ids(c(current_completed, input$completed_assess_id))
  })
  
  # Export functionality
  output$export_data <- downloadHandler(
    filename = function() {
      paste("delist-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(global_delist_candidates1, file, row.names = FALSE)
    }
  )
  
  
  # Toolbar UI
	output$toolbarUI=renderUI({
		column(4,fixedPanel(draggable=T, style="z-index:1000; overflow-y:scroll; max-height: 85vh",
			shinyjqui::jqui_resizable(bsCollapse(multiple=T, open=1,
				bsCollapsePanel(list(icon('plus-circle'), icon('edit'), 'Review'), value=1,
					fluidRow(
						textInput('param_comment', 'Parameter Review Comment'),
						actionButton('comment_save', 'Save Comment', 
						             style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('toolbox'))
					 ),fluidRow(
					  actionButton('mark_complete', label = "AU Review Complete", 
					               style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'))
			    ,fluidRow(
					  downloadButton('exp_rev', label = "Export reviews", 
					                 style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'))
				              )))))
	})
	
	observeEvent(input$comment_save, {
	  if (is.null(input$dt1_rows_selected)) {
	    showNotification("Please select a row before saving a comment", type = "error")
	    return()
	  }
	  req(input$dt1_rows_selected)
	  selected_row <- input$dt1_rows_selected
	  comment <- input$param_comment
	  
	  # Update the Comments column
	  global_delist_candidates1[selected_row, "Comments"] <<- comment
	  
	  # Optionally: clear the comment input field after saving
	  updateTextInput(session, "param_comment", value = "")
	})
  }

## run app
shinyApp(ui = ui, server = server)
