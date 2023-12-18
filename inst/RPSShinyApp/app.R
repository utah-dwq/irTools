
library(shiny)
library(ggplot2)
library(readxl)
library(leaflet)
library(sf)
library(rmapshaper)
library(ggrepel)
library(rsconnect)

load(system.file("extdata", "HUC12_Shapefile/Watersheds_Area.shp", package = "irTools"))
load(system.file("extdata","RPS_summary.xlsx", package = "irTools"))

data_long <- read_excel("RPS_summary.xlsx",sheet="SUMMARY")

# Load your shapefile (replace 'your_shapefile.shp' with the actual path)
#shapefile_path <- 'HUC12_Shapefile/Watersheds_Area.shp'
shapefile_path <- 'Watersheds_Area.shp'
geo_data <- st_read(shapefile_path, stringsAsFactors = FALSE)

# Merge with RPI data
geo_data <- merge(geo_data, data_long[, c("HUC_12", "Basin", "RPI")], by.x = "HUC_12", by.y = "HUC_12")

# Simplify the geometry for better performance
geo_data <- ms_simplify(geo_data, keep = 0.01)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-input {
        font-size: 12px; /* Adjust the font size as needed */
      }
    "))
  ),
  selectInput("basin", "Select Basin", choices = unique(data_long$Basin)),
  selectInput("scenario", "Select Scenario", choices = unique(data_long$Scenario)),
  plotOutput("bubble_plot"),
  # leafletOutput("map")
)

# Define server
server <- function(input, output) {
  output$bubble_plot <- renderPlot({
    # Use input$scenario to filter and update the plot
    selected_basin <- input$basin
    selected_scenario <- input$scenario
    filtered_data <- subset(data_long, Scenario == selected_scenario & Basin == selected_basin)
    
    # Calculate median values for x and y
    median_x <- median(filtered_data$Stressor_Index)
    median_y <- median(filtered_data$Ecological_Index)
    
    # Plot the bubble plot using ggplot2
    ggplot(filtered_data, aes(x = Stressor_Index, y = Ecological_Index, size = Social_Index, color = Watershed, label = Watershed)) +
      geom_vline(xintercept = median_x, linetype = "solid", color = "slategray3", linewidth = 1.5, show.legend = TRUE) +
      geom_hline(yintercept = median_y, linetype = "solid", color = "slategray3", linewidth = 1.5, show.legend = TRUE) +
      geom_point() +
      geom_text_repel(min.size = 3.5, size = 3.5, max.overlaps = Inf, fontface="bold") +  # Increase min.size and overall text size
      labs(title = paste("Scenario", selected_scenario),
           x = "Stressor Index", y = "Ecological Index") +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      guides(
        color = "none",
        size = "none",
        linetype = guide_legend(override.aes = list(size = 1)))+
      annotate("text", x = median(filtered_data$Stressor_Index), y = max(filtered_data$Ecological_Index)+40, 
               label = "Potential \n priority areas", hjust = 1.1, vjust = 0.8,color="slategray3",size=6) +
      annotate("text", x = median(filtered_data$Stressor_Index), y = max(filtered_data$Ecological_Index)+40, 
               label = "Areas needing \n restoration", hjust = -0.1, vjust = 0.8,color="slategray3",size=6) +
      annotate("text", x = median(filtered_data$Stressor_Index), y = min(filtered_data$Ecological_Index)-40, 
               label = "Areas under \n significant stress", hjust = 1.1, vjust = 0,color="slategray3",size=6) +
      annotate("text", x = median(filtered_data$Stressor_Index), y = min(filtered_data$Ecological_Index)-40, 
               label = "Potentially less critical areas \n or More data needed", hjust = -0.1, vjust = 0,color="slategray3",size=6)
  })
  
  #  output$map <- renderLeaflet({
  #   # Create a choropleth map
  #  leaflet(geo_data) %>%
  #   addProviderTiles("Stamen.Terrain") %>%
  #  addPolygons(fillColor = ~colorRamp(RPI)(RPI),
  #             fillOpacity = 0.7,
  #            color = "white",
  #           weight = 1,
  #          label = ~HUC_12,
  #         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
  #                                    textsize = "15px",
  #                                   direction = "auto"))
  #})
}

# Run the app
shinyApp(ui, server)
