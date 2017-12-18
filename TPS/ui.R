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
      
      # Input: Select a file
      fileInput("files_1",
                label = '1. Upload .html files',
                multiple = TRUE,
                accept = ".html"),
      
      # Upload summary results text
      textOutput('result'),
      
      # Horizontal line 
      tags$hr(),
      
      h5(tags$b('2. Select table row(s) to view in the map')),
      
      # Horizontal line
      tags$hr(),
      
      h5(tags$b('3. Export all clipped data')),

      # Download button
      downloadButton('downloadShp', 'Download')
      
    ),
    
  
    ### Main Output
    fluidRow(
      
      # Left-hand column
      column(width = 7,
        
        # Map Output
        leafletOutput("vzmap", height = 300),
        
        # Results Table
        DT::dataTableOutput("contents")
    
      )
    )
  )
)