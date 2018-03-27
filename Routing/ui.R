#############################
# Corridor Geocoder UI Code #
#############################

### Libraries
library(shiny)
library(DT)
library(leaflet)

### Load Input Vars
la_nodes <- readRDS('data/nodes.rds')

### User Interface
ui <- fluidPage(
  
  ### Title
  titlePanel("Corridor Geocoder"),
  
  fluidRow(
    column(4,
           wellPanel(
             # Select Intersection #1
             selectizeInput(inputId = "int1",
                            label = 'Start Intersection',
                            choices = la_nodes,
                            selected = NULL,
                            multiple = FALSE),
             
             # Select Intersection #2
             selectizeInput(inputId = "int2",
                            label = 'End Intersection',
                            choices = la_nodes,
                            selected = NULL,
                            multiple = FALSE),
             
             #h5(tags$b('Create a Corridor')),
             
             # Download button
             #downloadButton('downloadData', 'Download')
             actionButton("addEntry", "Add Corridor")
           )
    ),
    
    column(8,
           # Map Output
           leafletOutput("map", height = 300)
    )
  ),
  
  fluidRow(
    tabsetPanel(
      #tabPanel("Matched Runs", DT::dataTableOutput("matchtable")),
      tabPanel("Corridors", DT::dataTableOutput("pathtable"))
    )
  )
)
