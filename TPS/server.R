### Libraries
require(rvest)
library(magrittr)
library(stringr)
library(sf)
library(leaflet)
library(dplyr)
library(DT)

### Support Functions
# Function to buffer in Nad83 and Return in WGS84
geom_buff <- function(boundary, ft) {
  geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
  geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
  geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
  return(geom_wgs84)
}

# Function to process HTML data into spatial df
html_process <- function(datapath) {  
  
  # Read the HTML file
  html_data <- read_html(datapath)
  
  # Process html data
  html_data %>%
    html_nodes('script') %>%
    # Extract second 'script' node
    extract2(2) %>%
    html_text %>%
    # Regex match formula
    str_match_all('title: "(.*)"') %>%
    # Pull out content within ()
    extract2(1) -> lat_lng
  
  # Grab second column, split
  lat_lng <- strsplit(lat_lng[,2], ", ")
  
  # Convert to df, time
  df <- as.data.frame(do.call(rbind, lapply(lat_lng, rbind)))
  colnames(df) <- c("Lat", "Lng", "Timestamp")
  df$Timestamp <- as.POSIXct(strptime(df$Timestamp, format = "%d-%b-%y %H.%M.%S", "UTC"))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$Lng <- as.numeric(as.character(df$Lng))
  
  # Convert df to sf object
  sf_df <- st_as_sf(df,
                    coords=c("Lng","Lat"),
                    crs=4326,
                    remove=FALSE)
  return(sf_df)
  
}

# Function to filter points to project boundary
sf_points_filter <- function(spatial_df) {
  
  # Filter points within project boundary
  sf_df_filtered <- spatial_df[veniceblvd_buff,]
  
  # Only return if it has at least two points in project boundary
  if(nrow(sf_df_filtered) > 1){
    return(sf_df_filtered)
  } else {
    return(NULL)
  }
  
}

# Function to take spatial df, filter to project boundary, and create segments
segmentize <- function(spatial_df, fname) {
  
  # Create Polylines
  if(!is.null(spatial_df)){
    
    # Merge points into linestring
    lafd_path <- spatial_df %>%
      summarize(Venice.Begin = min(Timestamp),
                Venice.End = max(Timestamp),
                Time.Sec = difftime(max(Timestamp),min(Timestamp),units='secs'),
                do_union=FALSE) %>%
      st_cast("LINESTRING") %>%
      mutate(
        File = fname
      )
    
    # Add length and reorder columns
    lafd_path <- lafd_path %>%
      mutate(
        Meters = round(st_length(lafd_path),0)
      ) %>%
      select(
        File,
        Venice.Begin,
        Venice.End,
        Time.Sec,
        Meters
      )
    
    # Return resulting segment
    return(lafd_path)
  } else {
    return(NULL)
  }
  
}
 
### Load and Prep Data
veniceblvd <- st_read('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/data/eval_extent/tsp-extent_line.shp')
veniceblvd_buff <- geom_buff(veniceblvd, 100)

### Server Code
server <- function(input, output) {
  
  
  # Reactive expression to process html files to sf dfs
  lafd_points <- reactive({
    
    if(!is.null(input$files_1)) {
      
      # html process function for all uploaded html files
      lafd_points <- lapply(input$files_1$datapath, html_process)
      
      # add name of html file to list
      names(lafd_points) <- input$files_1$name
      
      # Filter out points outside the project boundary
      lafd_points <- lapply(lafd_points, sf_points_filter)
      
      # Subset out NULL values
      lafd_points <- lafd_points[-which(sapply(lafd_points, is.null))]
      
      # Return final list of sf dfs
      return(lafd_points)
      
    } else {
      return(NULL)
    }
    
  })
  
  # Reactive expression for converting points to segments
  lafd_paths <- reactive({
    
    if(!is.null(lafd_points())) {

      # Extract points and filenames
      pts <- lafd_points()
      names <- names(lafd_points())
      
      # Set simplify = FALSE to return list instead of matrix
      lafd_paths <- mapply(segmentize
                                  ,pts
                                  ,names
                                  ,SIMPLIFY = FALSE
                                  )
      
      # From sf dfs, get paths
      lafd_paths_df <- do.call(rbind, lafd_paths)
      
      # Return formatted df
      return(lafd_paths_df)
      
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to get points from selected rows
  lafd_points_s <- reactive({
    
    if(is.null(input$contents_rows_selected)){
      return(NULL)
    } else {
      
      # Get selected rows
      s <- input$contents_rows_selected
      
      # Filter data
      lafd_points_s <- lafd_points()
      lafd_points_s <- lafd_points_s[[s]]
      
      # Return filtered data
      return(lafd_points_s)
    }
  })
  
  # Reactive expression to get paths from selected rows
  lafd_paths_s <- reactive({

    # If null, return null, else get selected row(s)
    if(is.null(input$contents_rows_selected)){
      return(NULL)
    } else {

      # Get selected rows
      s <- input$contents_rows_selected
      
      # Filter data
      lafd_paths_df_s <- lafd_paths()
      lafd_paths_df_s <- lafd_paths_df_s[s,]
      
      # Return filtered data
      #print(lafd_paths_df_s)
      return(lafd_paths_df_s)
    }
    
  })
  
  # Text Summarizing Clipping Results
  output$result <- renderPrint({
    
    # Input variables
    upload_ct <- nrow(input$files_1)
    venice_ct <- nrow(lafd_paths()) #having trouble with this one
    
    # Output text
    if (!is.null(input$files_1)) {
      cat('Of the total '
          ,upload_ct
          ,' trips uploaded, '
          ,venice_ct
          ,' trips have segments within the project area.')
    }
    
  })
  
  # Data Table Output
  output$contents <- DT::renderDataTable({
    
    #print(str(lafd_points()))
    #hi <- lafd_points()
    #print(hi[[1]])
    #hi <- lafd_points_s()
    #print(hi)
    
    #print(lafd_points()[[0]])
    
    # input$files_1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    # If not null, process files and output
    if(!is.null(input$files_1)) {
      
      # Format time, remove geometry column
      lafd_paths_tbl <- lafd_paths() %>%
        mutate(
          Venice.Begin = as.character(Venice.Begin),
          Venice.End = as.character(Venice.End)
        ) %>%
        st_set_geometry(NULL)
      
      # Return formatted table
      return(lafd_paths_tbl)
      
    } else {
      return(NULL)
    }
    
  },
  # Restrict it to only one row selected at a time
  selection='single')
  
  # Render the Leaflet Map (based on reactive map object)
  output$vzmap <- renderLeaflet({
    
    # Map object
    map <- leaflet() %>%
      
      # Add stamen tileset - Toner Lite
      addProviderTiles(providers$Stamen.TonerLite)
    
    # If there are paths within Venice Blvd area, add to map
    if(!is.null(lafd_paths_s())) {
      map <- map %>% 
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = lafd_paths_s() 
      ) %>%
        addMarkers(
          data = lafd_points_s()
        )
    }
    
    # Return final map
    map
    
  })
  
  # Shapefile Output Downloader
  output$downloadShp <- downloadHandler(
    filename = 'lafdTripExport.zip',
    content = function(file) {
      if (length(Sys.glob("lafdTrip.*"))>0){
        file.remove(Sys.glob("lafdTrip.*"))
      }
      # using sf instead of sp package
      #writeOGR(SHP, dsn="lafdTrip.shp", layer="lafdTrip", driver="ESRI Shapefile")
      st_write(lafd_paths(), dsn = "lafdTrip.shp", layer = "lafdTrip", driver = "ESRI Shapefile")
      # i can add the csv later if i can get the shp working
      #write.csv(as.data.frame(cbind(SHP@data, as.data.frame(SHP@coords))), "lafdTrip.csv")
      zip(zipfile='lafdTripExport.zip', files=Sys.glob("lafdTrip.*"))
      file.copy("lafdTripExport.zip", file)
      if (length(Sys.glob("lafdTrip.*"))>0){
        file.remove(Sys.glob("lafdTrip.*"))
      }
    }
  )
  
}