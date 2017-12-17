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
html_process <- function(fname, datapath) {
  
  #print(htmlfile)
  
  # Read the HTML file
  #fname <- htmlfile$name
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
  # Filter points within project boundary
  sf_df_filtered <- sf_df[veniceblvd_buff,]
  
  # Create Polylines
  if(nrow(sf_df_filtered) > 1){
    
    # Merge points into linestring
    lafd_path <- sf_df_filtered %>%
      summarize(Venice.Begin = min(Timestamp),
                Venice.End = max(Timestamp),
                Time.Sec = difftime(max(Timestamp),min(Timestamp),units='secs'),
                do_union=FALSE) %>%
      st_cast("LINESTRING") %>%
      mutate(
        filename = fname
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
  
  # Reactive expression for data processing when html files are present
  lafd_paths <- reactive({
    
    if(!is.null(input$files_1)) {
      
      # Run each file through function, remove NULL values, combine into sf df
      lafd_paths <- apply(input$files_1[,c('name','datapath')], 1, function(y) html_process(y['name'],y['datapath']))
      lafd_paths <- lafd_paths[-which(sapply(lafd_paths, is.null))]
      lafd_paths_df <- do.call(rbind, lafd_paths)
      
      # Return formatted df
      return(lafd_paths_df)
      
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to create a shapefile
  # createShp <- reactive({
  #   myXY <- input$inputdata
  #   if (is.null(myXY)){
  #     return(NULL)      
  #   } else {
  #     xyPoints <- read.table(myXY$datapath, sep=",", header=T)
  #     
  #     shp <- SpatialPointsDataFrame(coords= cbind(xyPoints[,1:2]), data =  xyPoints)
  #     proj4string(shp) <- CRS("+init=epsg:4326")
  #     return(shp)
  #   }
  # })

  output$contents <- renderTable({
    
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
    
    
    
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
    
  })
  
  # Render the Leaflet Map (based on reactive map object)
  output$vzmap <- renderLeaflet({
    
    # Map object
    map <- leaflet() %>%
      
      # Add stamen tileset - Toner Lite
      addProviderTiles(providers$Stamen.TonerLite)
    
    # If there are paths within Venice Blvd area, add to map
    if(!is.null(lafd_paths())) {
      map <- map %>% 
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = lafd_paths() 
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