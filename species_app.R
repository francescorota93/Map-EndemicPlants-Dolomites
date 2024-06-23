# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(terra)
library(rmarkdown)
library(geojsonsf)
library(sf)
# Load occurrence data
# Assuming a CSV file "species_occurrences.csv" with columns: species, latitude (y), longitude (x)
occurrences <- read.table("/home/francesco/Documents/species_app/endemic_dolo50.txt", header = TRUE)

### transform to wgs84
occ_utm32N <- vect(occurrences, geom=c("x", "y"), crs = "epsg:32632")

occ_wgs84 <- project(occ_utm32N, "EPSG:4326")
# Define UI
ui <- fluidPage(
  titlePanel("Endemic Species of the Dolomites Database"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", choices = unique(occurrences$species)),
      actionButton("update", "Update Map"),
      br(),
      textOutput("aoo_text"),
      textOutput("eoo_text")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  observeEvent(input$update, {
    req(input$species)
    
    occ_wgs84_df <- as.data.frame(occ_wgs84, geom ="XY")
    # Filter data based on selected species
    species_data <- occ_wgs84_df %>%
      filter(species == input$species)
    
    # Calculate Area of Occurrence (AOO)
    aoo_points <- species_data %>%
      distinct(y, x)
    
    # Convert data to SpatVector
    coords <- cbind(aoo_points$x, aoo_points$y)
    coords1 <- vect(coords, type="points", crs="epsg:4326")
    # Calculate Extent of Occurrence (EOO)
    eoo_hull <- convHull(coords1)
    
    # Calculate areas
    aoo_area <- length(coords1) * (2 * 2) # Assuming a 2 km x 2 km grid for AOO
    eoo_area <- expanse(eoo_hull, unit = "km")
    
    # Convert hull to GeoJSON for leaflet
    eoo_hull_geojson <- geojsonsf::sf_geojson(st_as_sf(eoo_hull))
    
    # Render map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = species_data,
          lat = ~y,
          lng = ~x,
          color = "blue",
          radius = 5,
          popup = ~paste("Species:", species)
        ) %>%
        addGeoJSON(
          geojson = eoo_hull_geojson,
          color = "red",
          weight = 2,
          fill = FALSE#,
          #label = "Extent of Occurrence"
        )
    })
    # Display AOO and EOO areas in the UI
    output$aoo_text <- renderText({
      paste("Area of Occurrence (AOO):", round(aoo_area, 2), "km²")
    })
    
    output$eoo_text <- renderText({
      paste("Extent of Occurrence (EOO):", round(eoo_area, 2), "km²")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
