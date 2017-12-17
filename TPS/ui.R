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
      
      # Input: Select a file
      fileInput("files_1",
                label = "Select all .html files",
                multiple = TRUE,
                accept = ".html"),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Checkbox if file has header
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes 
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
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