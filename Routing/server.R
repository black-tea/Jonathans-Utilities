#################################
# Corridor Geocoder Server Code #
#################################

### Load Libraries
library(stplanr)
library(leaflet)
library(sp)
library(sf)
library(DT)

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
  
  # Append path to dataframe
  pathTbl <- eventReactive(input$addEntry, {
    # If pathTbl doesn't yet exist, create it
    if(!exists("pathTbl")) {
      pathTbl <- shortpath()
    } else{
      # Otherwise, append path to pathTbl
      pathTbl <- rbind.data.frame(pathTbl(),shortpath())
    }
    return(pathTbl)
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
      print(st_bbox(shortpath))
      
      print(st_bbox(shortpath)[1])
      # Erase shapes for new path
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = shortpath
        ) %>%
        # Update the map zoom bounds
        fitBounds(lng1 = as.double(st_bbox(shortpath)[1]),
                  lat1 = as.double(st_bbox(shortpath)[2]),
                  lng2 = as.double(st_bbox(shortpath)[3]),
                  lat2 = as.double(st_bbox(shortpath)[4])
        )
    }
  })
  
  ### UI Table Output
  output$pathtable <- DT::renderDataTable({
    
    # If not null, process files and output
    if(exists("pathTbl")) {
      
      pathTbl <- pathTbl()
      
      return(pathTbl)
      
    } else {
      
      return(NULL)
    }
    
  },
  # Restrict it to only one row selected at a time
  selection = 'single',
  rownames = FALSE
  )
  
}