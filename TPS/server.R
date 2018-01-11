########################################
# LAFD / TPS Data Cleaning Server Code #
########################################

### Libraries
require(rvest)
library(magrittr)
library(stringr)
library(sf)
library(leaflet)
library(dplyr)
library(DT)
library(fuzzyjoin)
library(IRanges)
library(lubridate)

options(tibble.print_max = Inf)

### Support Functions
# Function to buffer in Nad83 and Return in WGS84
geom_buff <- function(boundary, ft) {
  geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
  geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
  geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
  return(geom_wgs84)
}

# Function to process TPS csv file into a spatial df
tps_process <- function(datapath) {
  
  # Read in Loop Data
  loop_data <- read.csv(datapath,
                        header = TRUE,
                        sep = ',',
                        stringsAsFactors = FALSE
  )
  
  print("pre pre")
  print(loop_data)
  
  loop_data <- loop_data %>%
    # Convert timestamp to date/time value
    mutate(RECID = as.numeric(RECID),
           # lubridate package to supply vector of possible datetime character formats
           TIMESTMP = parse_date_time(TIMESTMP,
                                      orders = c("%m/%d/%Y %H:%M:%S",
                                                 "%Y-%m-%d %H:%M:%S"),
                                      tz="America/Los_Angeles"),
           CON_ID = as.numeric(CON_ID),
           DET_ID = as.numeric(DET_ID),
           TAG_ID = as.numeric(TAG_ID),
           ERRORCODE = as.numeric(ERRORCODE),
           ERRORMSG = as.character(ERRORMSG),
           LATENESS = as.numeric(LATENESS),
           SYSID = as.numeric(SYSID),
           VEH_NUM = as.character(VEH_NUM),
           EVFLAG = as.numeric(EVFLAG)) %>%
    # Filter out erroneous dates created by null date values
    filter(TIMESTMP > "2017-01-01")
  
  print("pre filter")
  print(loop_data)
    
    
  loop_data <- loop_data %>%
    # Join to detector location information
    left_join(detectors, by='DET_ID') %>%
    # Only want "ISSUED" error codes (68111, 68112); also remove lat/lon NA values
    mutate(
      lat = as.numeric(lat),
      lon = as.numeric(lon)
    ) %>%
    filter(ERRORCODE %in% c(68111,68112)
           ,!is.na(lat)
           ,!is.na(lon))

  print("post filter")
  print(loop_data)
  
  # Convert df to sf object
  loop_sf <- st_as_sf(loop_data,
                      coords=c("lon","lat"),
                      crs=4326,
                      remove=FALSE)
  
  return(loop_sf)
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
  
  # Convert to df, import / convert time to Los Angeles tz
  df <- as.data.frame(do.call(rbind, lapply(lat_lng, rbind)))
  colnames(df) <- c("Lat", "Lng", "Timestamp")
  df$Timestamp <- as.POSIXct(df$Timestamp,
                             format = "%d-%b-%y %H:%M:%S",
                             tz = "UTC")
  # commenting out timezone conversion, since they are now giving it in LA timezone
  attributes(df$Timestamp)$tzone <- "America/Los_Angeles"  
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

# Function to create segments
segmentize <- function(spatial_df, fname) {
  
  # Create Polylines
  if(!is.null(spatial_df)){
    
    # Merge points into linestring
    lafd_path <- spatial_df %>%
      summarize(start = min(Timestamp),
                end = max(Timestamp),
                Time.Sec = difftime(max(Timestamp),min(Timestamp),units='secs'),
                do_union=FALSE) %>%
      st_cast("LINESTRING") %>%
      mutate(
        File = fname
      )
    
    # Add length and reorder columns
    lafd_path <- lafd_path %>%
      mutate(
        Miles = round((st_length(st_transform(lafd_path, 2229))/5280),2),
        MPH = round((Miles/(Time.Sec/3600)),2)
      ) %>%
      select(
        File,
        start,
        end,
        Miles,
        MPH
      )
    
    # Return resulting segment
    return(lafd_path)
  } else {
    return(NULL)
  }
  
}

# Function to create Icons for map
createIcon <- function(color) {
  
  custom_icon <- awesomeIcons(
    icon = 'circle-o',
    iconColor = '#ffffff',
    library = 'fa',
    markerColor = color
  )
  
  return(custom_icon)
  
}
 
### Load and Prep Other Data
# Venice Boundary
veniceblvd <- st_read('data/eval_extent/tsp-extent_line.shp')
veniceblvd_buff <- geom_buff(veniceblvd, 100)
# Loop Detector Location 
detectors <- read.csv('data/signal/detectors.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)

### Server Code
server <- function(input, output) {
  
  # Reactive expression to process LADOT TPS logs
  tps_log <- reactive({
    
    if(!is.null(input$tps_files)) {
      
      # Process each TPS Log
      tps_log <- lapply(input$tps_files$datapath, tps_process)
      
      # Combine list of dfs into one sf df
      tps_log <- do.call(rbind, tps_log)
      
      tps_log <- tps_log %>%
        arrange(TIMESTMP) %>%
        # Calculate lag time between each timestamp and one before it
        mutate(TIMESTMP_LAG = ifelse(!is.na(lag(TIMESTMP)),
                                     TIMESTMP - lag(TIMESTMP),
                                     0)) %>%
        # Create break point where there is 3 min gap btw last event, assign Run ID
        mutate(run_flag = ifelse(TIMESTMP_LAG > 180, 1, 0),
               run_id = 1 + cumsum(run_flag) )

      return(tps_log)
    }
  })
  
  # Reactive expression to convert tps_logs() into "runs"
  tps_runs <- reactive({
    
    # Group by run, get start/end
    tps_runs <- tps_log() %>%
      group_by(run_id) %>%
      summarise(
        start = min(TIMESTMP),
        end = max(TIMESTMP)
      ) %>%
      st_set_geometry(NULL)
    
    return(tps_runs)
  })
  
  # Reactive expression to process html files to list of sf dfs
  lafd_points <- reactive({
    
    if(!is.null(input$lafd_files)) {
      
      # html process function for all uploaded html files
      lafd_points <- lapply(input$lafd_files$datapath, html_process)
      
      # add name of html file to list
      names(lafd_points) <- input$lafd_files$name
      
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
  
  # Reactive expression for converting LAFD points to segments
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
  
  ### All Reactive expressions involving the table joins
  # First table join combining LADOT & LAFD Data
  join_tbl <- reactive({

    # Only process if both LAFD & LADOT data is not null
    if((!is.null(tps_runs()))&(!is.null(lafd_points()))) {
      
      # Debugging
      print(tps_runs())
      print(lafd_paths())
      
      # Format time, remove geometry column
      lafd_paths_tbl <- lafd_paths() %>%
        st_set_geometry(NULL) 

      # Group by run, get start/end
      join_tbl <- tps_runs() %>%
        # interval join from 'fuzzyjoin' package to identify overlap between time intervals
        interval_full_join(lafd_paths_tbl)
      
      # Return joined table
      return(join_tbl)

      } else {
      return(NULL)
    }

  })
  
  # Post-Join match table
  match_tbl <- reactive({
    
    match_tbl <- join_tbl() %>%
      filter(!is.na(run_id)) %>%
      filter(!is.na(File)) %>%
      mutate(
        start.x = as.character(start.x),
        end.x = as.character(end.x),
        start.y = as.character(start.y),
        end.y = as.character(end.y)
      ) %>%
      dplyr::rename('TPS Run ID' = 'run_id') %>%
      dplyr::rename('TPS Start' = 'start.x') %>%
      dplyr::rename('TPS End' = 'end.x') %>%
      dplyr::rename('LAFD File' = 'File') %>%
      dplyr::rename('LAFD Start' = 'start.y') %>%
      dplyr::rename('LAFD End' = 'end.y')
    
    # Return formatted table
    return(match_tbl)
    
  })
  
  # Post-Join LAFD table
  lafd_tbl <- reactive({
    
    lafd_tbl <- join_tbl() %>%
      filter(is.na(run_id)) %>%
      select(File, start.y, end.y, Miles, MPH) %>%
      mutate(
        start.y = as.character(start.y),
        end.y = as.character(end.y)
      ) %>%
      dplyr::rename('File' = 'File') %>%
      dplyr::rename('Start' = 'start.y') %>%
      dplyr::rename('End' = 'end.y')
    
    # Return formatted table
    return(lafd_tbl)
    
  })
  
  # Post-Join LADOT TPS table
  tps_tbl <- reactive({
    
    tps_tbl <- join_tbl() %>%
      filter(is.na(File)) %>%
      select(run_id, start.x, end.x) %>%
      mutate(
        start.x = as.character(start.x),
        end.x = as.character(end.x)
      ) %>%
      dplyr::rename('Run ID' = 'run_id') %>%
      dplyr::rename('Start' = 'start.x') %>%
      dplyr::rename('End' = 'end.x')
    
    # Return formatted table
    return(tps_tbl)
    
  })
  

  
  ### UI Text Output
  # Text Summarizing Clipping Results
  output$result <- renderPrint({
    
    # Input variables
    upload_ct <- nrow(input$lafd_files)
    venice_ct <- nrow(lafd_paths()) 
    
    # Output text
    if (!is.null(input$lafd_files)) {
      cat('Of the total '
          ,upload_ct
          ,' LAFD trips uploaded, '
          ,venice_ct
          ,' trips have segments within the project area.')
    }
    
  })
  
  ### UI Table Output
  # Matched Rows
  output$matchtable <- DT::renderDataTable({

    # If not null, process files and output
    if((!is.null(input$lafd_files))&(!is.null(input$tps_files))) {
      
      return(match_tbl())
      
    } else {
      return(NULL)
    }
    
  },
  # Restrict it to only one row selected at a time
  selection='single')
  
  # Unmatched LAFD Table
  output$lafdtable <- DT::renderDataTable({
    
    # Process once both inputs have been uploaded
    if((!is.null(input$lafd_files))&(!is.null(input$tps_files))) {
      
      return(lafd_tbl())
      
    } else {
      return(NULL)
    }
    
  },
  # Restrict it to only one row selected at a time
  selection='single')
  
  # Unmatched TPS Logs
  output$tpstable <- DT::renderDataTable({
    
    # Process once both inputs have been uploaded
    if((!is.null(input$lafd_files))&(!is.null(input$tps_files))) {
      
      return(tps_tbl())
      
    } else {
      return(NULL)
    }
    
  },
  # Restrict it to only one row selected at a time
  selection='single')
  

  
  ### Reactive Expressions to get data from selected rows
  # List of all points/paths from selected rows of matched data
  join_pts_s <- reactive({
    if(!is.null(input$matchtable_rows_selected)){
      
      # Get selected rows
      row_num <- input$matchtable_rows_selected
      
      # TPS Points
      tps_run_num <- as.integer(match_tbl()[row_num, 1])
      tps_points_s <- tps_log() %>%
        filter(run_id == tps_run_num)
      
      # LAFD Points
      lafd_map <- as.character(match_tbl()[row_num, 4])
      lafd_points_s <- lafd_points()[[lafd_map]]
      
      # LAFD Paths
      lafd_paths_df_s <- segmentize(lafd_points_s, lafd_map)
      
      # Create list of data
      join_pts_s <- list(
        tps_points_s = tps_points_s,
        lafd_points_s = lafd_points_s,
        lafd_paths_df_s = lafd_paths_df_s
      )
      
      return(join_pts_s)
      
    } else {
      return(NULL)
    }
  })
  
  # LAFD points from selected rows
  lafd_points_s <- reactive({
    
    if(is.null(input$lafdtable_rows_selected)){
      return(NULL)
    } else {
      
      # Get selected rows & grab LAFD Points
      row_num <- input$lafdtable_rows_selected
      lafd_map <- as.character(lafd_tbl()[row_num, 1])
      lafd_points_s <- lafd_points()[[lafd_map]]
      
      # Return filtered data
      return(lafd_points_s)
    }
  })
  
  # LAFD paths from selected rows
  lafd_paths_s <- reactive({
    
    # If null, return null, else get selected row(s)
    if(is.null(input$lafdtable_rows_selected)){
      return(NULL)
    } else {
      
      # Get selected rows
      row_num <- input$lafdtable_rows_selected
      lafd_map <- as.character(lafd_tbl()[row_num, 1])
      lafd_points_s <- lafd_points()[[lafd_map]]
      
      # Filter data
      lafd_paths_df_s <- segmentize(lafd_points_s, lafd_map)
      
      # Return filtered data
      return(lafd_paths_df_s)
    }
    
  })
  
  # TPS points from selected rows
  tps_points_s <- reactive({
    
    if(is.null(input$tpstable_rows_selected)){
      return(NULL)
    } else {
      
      # Get selected rows, then run # from joined table
      row_num <- input$tpstable_rows_selected
      run_num <- as.integer(tps_tbl()[row_num,1])
      
      # Grab points from selected run #
      tps_points_s <- tps_log() %>%
        filter(run_id == run_num)
      
      return(tps_points_s)
    }
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

    if(!is.null(input$matchtable_rows_selected)) {

      #lafd_points_s <- lafd_points_s()
      join_pts_s <- join_pts_s()

      # Erase markers/shapes and add new lafd ones
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = join_pts_s$lafd_paths_df_s
        ) %>%
        addAwesomeMarkers(
          data = join_pts_s$lafd_points_s
          ,icon = createIcon('red')
          ,label = as.character(join_pts_s$lafd_points_s$Timestamp)
        ) %>%
        addAwesomeMarkers(
          data = join_pts_s$tps_points_s
          ,icon = createIcon('blue')
          ,label = ~as.character(join_pts_s$tps_points_s$TIMESTMP)
        ) %>%
        # Update the map zoom bounds
        fitBounds(lng1 = max(join_pts_s$lafd_points_s$Lng),
                  lat1 = max(join_pts_s$lafd_points_s$Lat),
                  lng2 = min(join_pts_s$lafd_points_s$Lng),
                  lat2 = min(join_pts_s$lafd_points_s$Lat)
        )
    }
  })
  
  # Observer focused on Unmatched LAFD Points
  observe({
    
    if(!is.null(input$lafdtable_rows_selected)) {
      
      lafd_points_s <- lafd_points_s()
      
      # Erase markers/shapes and add new lafd ones
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          color = '#fc0307',
          weight = 3,
          opacity = 1,
          data = lafd_paths_s()
        ) %>%
        addAwesomeMarkers(
          data = lafd_points_s
          ,icon = createIcon('red')
          ,label = as.character(lafd_points_s$Timestamp)
        ) %>%
        # Update the map zoom bounds
        fitBounds(lng1 = max(lafd_points_s$Lng),
                  lat1 = max(lafd_points_s$Lat),
                  lng2 = min(lafd_points_s$Lng),
                  lat2 = min(lafd_points_s$Lat)
                  )
    }
  })
  
  # Observer focused on Unmatched TPS Points
  observe({
    
    if(!is.null(input$tpstable_rows_selected)) {
      
      tps_points_s <- tps_points_s()
      
      # Erase markers/shapes and add new tps ones
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          data = tps_points_s
          ,icon = createIcon('blue')
          ,label = ~as.character(TIMESTMP)
        ) %>%
        #Update the map zoom bounds
        fitBounds(lng1 = max(tps_points_s$lon),
                  lat1 = max(tps_points_s$lat),
                  lng2 = min(tps_points_s$lon),
                  lat2 = min(tps_points_s$lat)
        )
    }
  })
  
  #### Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(join_tbl(), file)
    }
  )
  
}