library(shiny)
library(leaflet)
library(dplyr)
library(terra)
library(sf)
library(red)

# Load occurrence data
occurrences <- read.table("data/endemic_dolo50_v1.txt", sep = "\t", header = TRUE)

# Load species information
species_info <- read.csv("data/tab_spec_info.csv")

# Join species information with occurrences
occurrences <- occurrences %>%
  left_join(species_info, by = "species")

# Transform to spatial data (EPSG:32632)
occ_spatial <- vect(occurrences, geom = c("x", "y"), crs = "epsg:32632")

# UI definition
ui <- fluidPage(
  titlePanel("Endemic Plants of the Dolomites"),
  p("Rota, F., Casazza, G., Genova, G. et al. Topography of the Dolomites modulates range dynamics of narrow endemic plants under climate change. Sci Rep 12, 1398 (2022). https://doi.org/10.1038/s41598-022-05440-3"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", choices = unique(occurrences$species)),
      textOutput("aoo_text"),
      textOutput("eoo_text"),
      br(),
      h4("Species Information"),
      textOutput("species_name"),
      textOutput("family"),
      textOutput("life_form"),
      textOutput("dispersal_methods"),
      textOutput("habitat_directive"),
      textOutput("iucn_category")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Server definition
server <- function(input, output, session) {
  observeEvent(input$species, {
    req(input$species)
    
    # Filter data for the selected species
    species_data <- occurrences %>% filter(species == input$species)
    species_spatial <- occ_spatial[occurrences$species == input$species, ]
    
    # Transform to WGS84 for visualization
    species_wgs84 <- project(species_spatial, "epsg:4326")
    
    # Calculate EOO (convex hull)
    eoo_hull <- convHull(species_spatial)
    eoo_area <- expanse(eoo_hull, unit = "km")
    
    # Calculate AOO (create 2x2 km grid)
    grid_size <- 2000  # Grid size in meters
    bounds <- ext(species_spatial)
    grid <- rast(bounds, res = grid_size, crs = "epsg:32632") %>%
      as.polygons() %>%
      st_as_sf()
    
    # Identify occupied grid cells
    occupied_cells <- st_intersects(grid, st_as_sf(species_spatial), sparse = FALSE)
    occupied_grid <- grid[apply(occupied_cells, 1, any), ]
    occupied_grid_wgs84 <- st_transform(occupied_grid, crs = 4326)
    aoo_area <- nrow(occupied_grid) * (grid_size / 1000)^2  # AOO in km²
    aoo1 <- aoo(geom(species_spatial)[,c(3,4)])
    
    # Map preparation
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = occupied_grid_wgs84,
          color = "green",
          weight = 1,
          fillColor = "green",
          fillOpacity = 0.4,
          label = ~paste("Grid ID:", seq_len(nrow(occupied_grid_wgs84)))
        ) %>%
        addPolygons(
          data = st_as_sf(eoo_hull) %>% st_transform(crs = 4326),
          color = "red",
          weight = 2,
          fill = FALSE,
          label = "Extent of Occurrence (EOO)"
        )
    })
    
    # Update species information
    species_info_selected <- species_data[1, ]
    output$aoo_text <- renderText({
      paste("Area of Occupancy (AOO):", round(aoo1, 2), "km²")
    })
    output$eoo_text <- renderText({
      paste("Extent of Occurrence (EOO):", round(eoo_area, 2), "km²")
    })
    output$species_name <- renderText({
      paste("Taxonomic Name:", species_info_selected$Taxonomic.Name)
    })
    output$family <- renderText({
      paste("Family:", species_info_selected$Family)
    })
    output$life_form <- renderText({
      paste("Life Form:", species_info_selected$Life.Form)
    })
    output$dispersal_methods <- renderText({
      paste("Dispersal Methods:", species_info_selected$Dispersal.Methods)
    })
    output$habitat_directive <- renderText({
      paste("Habitat Directive:", species_info_selected$Habitat.Directive)
    })
    output$iucn_category <- renderText({
      paste("IUCN Category:", species_info_selected$IUCN.category)
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
