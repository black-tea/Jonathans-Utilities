#################################
# Corridor Geocoder Server Code #
#################################

### Load Libraries
library(stplanr)
library(leaflet)
library(sp)
library(sf)

### Load data from prep.R
la_network <- readRDS('data/la_network.rds')

# need to look at shiny observe event & action button for generating the line

### Server Code
server <- function(input, output, session) {
  
    # Shortest Path
    shortpath <- reactive({
      if((!is.null(input$int1))&(!is.null(input$int2))) {
        print(as.integer(input$int1))
        print(as.integer(input$int2))
        shortpath.sp <- sum_network_routes(la_network, as.integer(input$int1), as.integer(input$int2), sumvars = "length")
        shortpath.sf <- st_as_sf(shortpath.sp)
        # Return formatted table
        return(shortpath.sf)
        }
    })

  
  ### Map Object
  # Render the Leaflet Map (based on reactive map object)
  output$map <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite)
    # Return final map
    map
  })  
  
  ### Map Observer Objects
  # Observer focused on Matched Runs
  observe({
    if((!is.na(as.integer(input$int1)))&(!is.na(as.integer(input$int2)))) {
      shortpath <- shortpath()
      # Erase shapes for new path
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = shortpath)
        # ) %>%
        # # Update the map zoom bounds
        # fitBounds(lng1 = max(match_pts_s$lafd_points_s$lon),
        #           lat1 = max(match_pts_s$lafd_points_s$lat),
        #           lng2 = min(match_pts_s$lafd_points_s$lon),
        #           lat2 = min(match_pts_s$lafd_points_s$lat)
        # )
    }
  })
  
}