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

### Load Input Vars
#la_nodes <- unname(readRDS('data/nodes.rds')) # ISSUE: Names for LA Nodes is NULL!!!!
la_nodes <- readRDS('data/nodes.rds')
#la_nodes <- la_nodes[!is.null(names(la_nodes))]
la_nodes <- la_nodes[!is.na(la_nodes)]

# need to look at shiny observe event & action button for generating the line

### Server Code
server <- function(input, output, session) {
  
  # Shortest Path
  shortpath <- reactive({
    if(length(input$int1 > 1)&length(input$int2 > 1)) {
      int1 <- la_nodes[names(la_nodes) == input$int1]
      int1 <- as.integer(int1[!is.na(int1)])
      int2 <- la_nodes[names(la_nodes) == input$int2]
      int2 <- as.integer(int2[!is.na(int2)])
      shortpath.sp <- sum_network_routes(la_network, as.integer(int1), as.integer(int2), sumvars = "length")
      #shortpath.sp <- sum_network_routes(la_network, 38, 65, sumvars = "length")
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
    print(input$int1)
    if((input$int1 != "")&(input$int2 != "")) {
      print(input$int1)
      print(input$int2)
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
  
  # Int1 Selection
  #updateSelectizeInput(session, 'int1', choices = names(la_nodes), server = TRUE)
  #updateSelectizeInput(session, 'int2', choices = names(la_nodes), server = TRUE)
  
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