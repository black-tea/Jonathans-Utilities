library(shiny)
library(leaflet)

### User Interface
ui <- fluidPage(
  
  ### Title
  titlePanel("LAFD GPS Data Cleaning Utility"),
  
  ### Sidebar layout with input 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      #h5('Step 1: Upload .html files'),
      
      # Input: Select a file
      fileInput("files_1",
                label = 'Step 1: Upload .html files',
                multiple = TRUE,
                accept = ".html"),
      
      # Horizontal line 
      tags$hr(),
      
      h5(tags$b('Step 2: Export clipped data')),
      #tags$b('Step2: Export clipped .shp and .csv'),

      # Download button
      downloadButton('downloadShp', 'Download Shapefile')
      
    ),
    
  
    ### Main Output
    fluidRow(
      
      # Left-hand column
      column(width = 7,
        
        # Map Output
        leafletOutput("vzmap", height = 300),
        
        # Results Table
        tableOutput("contents")
    
      )
    )
  )
)