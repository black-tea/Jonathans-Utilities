####################################
# LAFD / TPS Data Cleaning UI Code #
####################################

### Libraries
library(shiny)
library(DT)
library(leaflet)

### User Interface
ui <- fluidPage(
  
  ### Title
  titlePanel("LAFD GPS Data Cleaning Utility"),
  
  ### Sidebar layout with input 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # LADOT TPS file input
      fileInput("tps_files",
                label = '1. Upload LADOT TPS logs',
                multiple = TRUE,
                accept = ".csv"),
      
      # LAFD .html file input
      fileInput("lafd_files",
                label = '2. Upload LAFD .html files',
                multiple = TRUE,
                accept = ".html"),
      
      # Upload summary results text
      textOutput('result'),
      
      # Horizontal line 
      tags$hr(),
      
      h5(tags$b('3. Select table row(s) to view in the map')),
      
      # Horizontal line
      tags$hr(),
      
      h5(tags$b('4. Export all clipped data')),

      # Download button
      downloadButton('downloadData', 'Download')
      
    ),
    
  
    ### Main Output
    fluidRow(
      
      # Left-hand column
      column(width = 7,
        
        # Map Output
        leafletOutput("map", height = 300),
        
        # Results Table
        #DT::dataTableOutput("contents"),
        tabsetPanel(
          #id = 'contents',
          tabPanel("Matched Runs", DT::dataTableOutput("matchtable")),
          tabPanel("Unmatched TPS Runs", DT::dataTableOutput("tpstable")),
          tabPanel("Unmatched LAFD Runs", DT::dataTableOutput("lafdtable"))
        )
    
      )
    )
  )
)