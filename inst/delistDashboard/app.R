#App to help visualize delisting candidates. There are many rows that have impaired sites that haven't been resampled for many cycles.

library(wqTools)
library(irTools)
library(leaflet)
library(shinyBS)
library(readxl)
library(DT) # for data tables
library(plotly)
library(sf)

asmntMap1=function(au_asmnt_poly, site_asmnt,target_site, dragging=T, ...){
  target_site$IR_Lat=wqTools::facToNum(target_site$IR_Lat)
  target_site$IR_Long=wqTools::facToNum(target_site$IR_Long)
  site_asmnt$IR_Lat=wqTools::facToNum(site_asmnt$IR_Lat)
  site_asmnt$IR_Long=wqTools::facToNum(site_asmnt$IR_Long)
  
  assessment_map <- 
    buildMap(plot_polys = T, search='', dragging=dragging, ...) %>%
    addMapPane("highlight", zIndex = 413) %>%
    leaflet::addCircleMarkers(data=site_asmnt, lat=~IR_Lat, lng=~IR_Long, group="Assessed sites",
                              color = "blue", opacity=0.8, layerId=~IR_MLID, options = pathOptions(pane = "markers"),
                              popup = paste0(
                                "IR MLID: ", site_asmnt$IR_MLID,
                                "<br> Site_cat: ", site_asmnt$Site_cat,
                                "<br> Site_cat22: ", site_asmnt$Site_cat_22
                              )
                              )%>%
    addCircleMarkers(data=target_site,lat=~IR_Lat,lng=~IR_Long, group= "Target Site",
       color = "red",opacity = 0.8,layerId=~IR_MLID,options=pathOptions(pane="markers"),
       popup=paste0("IR MLID: ", target_site$IR_MLID,
                     "IR MLNAME: ", "Target"))
                              
    assessment_map = assessment_map %>%
       leaflet::addLayersControl(position ="topleft",baseGroups = c("Topo","Satellite"),overlayGroups = c("Assessed sites", "Assessment units"),
         options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
         fitBounds(-114.0187, 37.02012, -109.0555, 41.99088) 
               return(assessment_map)	
}

ui <- fluidPage(
  titlePanel("Delisting AU-Parameter"),
  bsCollapse(id = "collapsible_panels", open = NULL, # You can set which panels are open by default
             bsCollapsePanel(title = "Data Upload", 
                             fileInput('delist_file1', 'Choose XLSX File', accept = c(".xlsx")),
                             textInput('target_mlid', 'Target MLID', ''),
                             textInput('target_latitude', 'Target Latitude', ''),
                             textInput('target_longitude', 'Target Longitude', ''),
                             actionButton('review', 'Review')
             ),
             bsCollapsePanel(title = "Data Table",
                             DTOutput('table') # Table output will go here
             ),
             bsCollapsePanel(title = "Map",
                             leafletOutput('map', height = "600") # Map output will go here
             )
             ))


server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  uploadedData <- reactiveVal()
  
  # Observe when a file is uploaded
  observeEvent(input$delist_file1, {
    data <- read_excel(input$delist_file1$datapath)
    uploadedData(data)
  })
  
  # Process the data and create a table output
  output$table = renderDT({
    req(input$delist_file1)
    req(uploadedData())
    data = uploadedData()
    data$index <- seq_len(nrow(data))
    data = data[!is.na(data$IR_Lat),]
    # Assuming the two columns are named 'assessmentUnitIdentifier' and 'R3172ParameterName'
    unique_data <- unique(data[, c('assessmentUnitIdentifier', 'R3172ParameterName','cycleFirstListed','cycleLastAssessed','Comment22')])
    datatable(unique_data, selection = 'single')
  })
  
  # Variable to store selected row
  selectedRow <- reactive({
    input$table_rows_selected
  })
  
  targetSite <- reactive({
    data.frame(IR_MLID=input$target_mlid, IR_Lat=as.numeric(input$target_latitude), IR_Long=as.numeric(input$target_longitude))
  })
       observeEvent(input$review,{
         # Map output
         output$map <- renderLeaflet({
           req(input$review,selectedRow(),uploadedData())
           uploaded_d <-uploadedData()
           assessed_sites <- uploaded_d[,c("assessmentUnitIdentifier","AU_NAME","IR_MLID","R3172ParameterName","Site_cat","Site_cat_22","IR_Lat","IR_Long")]
           # Find matching rows in the internal data
           data = uploaded_d[is.na(uploaded_d$Delist24)&!is.na(uploaded_d$IR_Lat),]
           # Assuming the two columns are named 'assessmentUnitIdentifier' and 'R3172ParameterName'
           unique_data <- unique(data[, c('assessmentUnitIdentifier', 'R3172ParameterName','cycleFirstListed','cycleLastAssessed','Comment22')])
           selected_data <- unique_data[selectedRow(), ]
           matched_data <- assessed_sites[assessed_sites$assessmentUnitIdentifier == selected_data$assessmentUnitIdentifier &
                                            assessed_sites$R3172ParameterName == selected_data$R3172ParameterName, ]
           print(selectedRow())
           print(selected_data)
           print(matched_data)
           assessment_map <- asmntMap1(
             au_asmnt_poly = NULL, # Assuming you do not have polygons for assessment units
             site_asmnt = matched_data,
             target_site = targetSite(),
             dragging = TRUE
             # ... any other arguments your function requires ...
           )
           # Return the map object to render it
           assessment_map
         })
       })              
         
}

shinyApp(ui, server)
