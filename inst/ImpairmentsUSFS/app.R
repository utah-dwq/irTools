library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(sf)

options(warn=0)

load("parameter_landowner_poly.Rdata")

ui <- fluidPage(
  titlePanel("USFS Land and Water Quality Impairments: pH, Temperature, Dissolved Oxygen"),
  HTML("The map displays USFS lands in blue, with options to view BLM, DNR, and NPS lands via 
       the layer control on the left. It highlights impairments for pH, Dissolved Oxygen, and 
       Temperature, with Assessment Units color-coded as follows: Yellow for one impaired 
       parameter, Orange for two, and Red for all three. Clicking on Assessment Units will display the impaired parameters.")
,
  fluidRow(column(12,shinycssloaders::withSpinner(leafletOutput("map",height="700px"),size=2, color="#0080b7")))
  
)

server <- function(input, output, session) {
  # filteredData <- reactive({
  #   land_ownership[land_ownership$AGENCY == input$agency, ]
  # })
  
  output$map <- renderLeaflet({
    blm_poly=st_simplify(blm_poly, dTolerance = 0.08) 
    nps_poly=st_simplify(nps_poly, dTolerance = 0.08) 
    dnr_poly=st_simplify(dnr_poly, dTolerance = 0.08) 
    
    review_map = leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE, dragging=TRUE))
    review_map=leaflet::addProviderTiles(review_map, "Esri.WorldTopoMap", group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
      addMapPane("au_poly", zIndex = 415)%>%
      addMapPane("land_owners", zIndex = 413)%>%
      addPolygons(data=nps_poly,group = "NPS",fillOpacity = 0.5,color="purple",weight = 3,options = pathOptions(pane = "land_owners"),
                  popup=paste0(
                    "AGENCY: ", nps_poly$AGENCY,
                    "<br> UT_LGD: ", nps_poly$UT_LGD)
      ) %>%
      
      addPolygons(data=usfs_poly,group = "USFS",fillOpacity = 0.5,color="blue",weight = 3,options = pathOptions(pane = "land_owners"),
                  popup=paste0(
                    "AGENCY: ", usfs_poly$AGENCY,
                    "<br> UT_LGD: ", usfs_poly$UT_LGD)
      ) %>%
      addPolygons(data=blm_poly,group = "BLM",fillOpacity = 0.6,color="purple",weight = 3,options = pathOptions(pane = "land_owners"),
                  popup=paste0(
                    "AGENCY: ", blm_poly$AGENCY,
                    "<br> UT_LGD: ", blm_poly$UT_LGD)
      ) %>%
      addPolygons(data=dnr_poly,group = "DNR",fillOpacity = 0.6,color="purple",weight = 3,options = pathOptions(pane = "land_owners"),
                  popup=paste0(
                    "AGENCY: ", dnr_poly$AGENCY,
                    "<br> UT_LGD: ", dnr_poly$UT_LGD)
      ) %>%
      addPolygons( data=au_poly_imp1,group="Assessment units",fillOpacity = 0.6, layerId=au_poly_imp1$polyID,
                   weight=3,color="orange",fillColor = ~color, options = pathOptions(pane = "au_poly"),
                   popup=paste0(
                     "AU name: ", au_poly_imp1$AU_NAME,
                     "<br> AU ID: ", au_poly_imp1$ASSESS_ID,
                     "<br> AU type: ", au_poly_imp1$AU_Type,
                     "<br> ",au_poly_imp1$label)
      )%>%leaflet::addLayersControl(position ="topleft",
                                    baseGroups = c("Topo"),overlayGroups = c("Assessment units", "NPS","USFS","BLM","DNR"),
                                    options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))%>%
      hideGroup(group = "BLM")%>%
      hideGroup(group="NPS")%>%
      hideGroup(group="DNR")%>%
      leaflet::addLegend(position = 'topright',
                         colors = c("yellow",'orange',"red",'blue',"purple"),
                         labels = c('One impaired parameter',"Two impaired parameters","All three parameters impaired",'USFS Lands',"NPS,BLM and DNR Lands"))
  })
}

shinyApp(ui = ui, server = server)
