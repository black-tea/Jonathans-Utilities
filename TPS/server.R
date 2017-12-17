### Libraries
require(rvest)
library(magrittr)
library(stringr)
library(sf)
library(leaflet)
library(dplyr)

### Support Functions
# Function to buffer in Nad83 and Return in WGS84
geom_buff <- function(boundary, ft) {
  geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
  geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
  geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
  return(geom_wgs84)
}

# Function to process HTML data into segments
html_process <- function(htmlfile) {
  
  # Read the HTML file
  html_data <- read_html(htmlfile)
  
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
  # Filter points within project boundary
  sf_df_filtered <- sf_df[veniceblvd_buff,]
  
  # Create Polylines
  if(nrow(sf_df_filtered) > 1){
    
    # Merge points into linestring
    lafd_path <- sf_df_filtered %>%
      summarize(minTime = min(Timestamp),
                maxTime = max(Timestamp),
                Time = (max(Timestamp)-min(Timestamp)),
                do_union=FALSE) %>%
      st_cast("LINESTRING")
    
    # Return resulting segment
    return(lafd_path)
  } else {
    return(NULL)
  }
  
}
 
### Load and Prep Data
#veniceblvd <- st_read('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/data/eval_extent/tsp-extent_line.shp')
#veniceblvd_buff <- geom_buff(veniceblvd, 100)

### Server Code
server <- function(input, output) {
  

  output$contents <- renderTable({
    
    # input$files_1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if(!is.null(input$files_1)) {
      
      # Run each file through function, remove NULL values, combine into sf df
      lafd_paths <- lapply(input$files_1$datapath, html_process)
      lafd_paths <- lafd_paths[-which(sapply(lafd_paths, is.null))]
      lafd_paths_df <- do.call(rbind, lafd_paths)
      lafd_paths_df <- lafd_paths_df %>% 
        mutate(
          minTime = as.POSIXlt(minTime),
          maxTime = as.POSIXlt(maxTime),
          Time = as.POSIXlt(Time),
          Length = "hi"
          # Here I want to attach the html filename so I can go back to it.
        ) %>%
        st_set_geometry(NULL)
      
      # Print output
      print(lafd_paths_df)
      return(lafd_paths_df)
      
    } else {
      return(NULL)
    }
    
    
    
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
    
  })
  
  # Render the Leaflet Map (based on reactive map object)
  output$vzmap <- renderLeaflet({
    
    map <- leaflet() %>%
      
      # Add stamen tileset - Toner Lite
      addProviderTiles(providers$Stamen.TonerLite) 
    
    # Return final map
    map
    
  })
  
}