#############################
# Corridor Geocoder UI Code #
#############################

### Libraries
library(shiny)
library(DT)
library(leaflet)

### Load Input Vars
#la_nodes <- unname(readRDS('data/nodes.rds')) # ISSUE: Names for LA Nodes is NULL!!!!
la_nodes <- readRDS('data/nodes.rds')
#la_nodes <- la_nodes[!is.null(names(la_nodes))]
la_nodes <- la_nodes[!is.na(la_nodes)]


### User Interface
ui <- fluidPage(
  
  ### Title
  titlePanel("Corridor Geocoder"),
  
  fluidRow(
    # NEED TO FIGURE OUT THIS COLUMN BELOW. Selectize inputs resulting in "NAs are not allowed in subscripted assignments". See la_nodes issue above
    column(4,
           wellPanel(
             # Select Intersection #1
             #uiOutput("int1Select"),
             selectizeInput(inputId = "int1",
                            label = 'Start Intersection',
                            choices = names(la_nodes),
                            #selected = NULL,
                            multiple = FALSE),

             # Select Intersection #2
             selectizeInput(inputId = "int2",
                            label = 'End Intersection',
                            choices = names(la_nodes),
                            #selected = NULL,
                            multiple = FALSE)#,
    # 
    #          #h5(tags$b('Create a Corridor')),
    # 
    #          # Download button
    #          #downloadButton('downloadData', 'Download')
    #          actionButton("addEntry", "Add Corridor")
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
