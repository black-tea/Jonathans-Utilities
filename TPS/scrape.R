require(rvest)
library(magrittr)
library(stringr)
library(sf)
library(leaflet)
library(dplyr)

### Function to buffer in Nad83 and Return in WGS84
geom_buff <- function(boundary, ft) {
  geom_nad83 <- st_transform(boundary, 2229) # Convert to NAD83
  geom_nad83 <- st_buffer(geom_nad83, ft) # Buffer
  geom_wgs84 <- st_transform(geom_nad83, 4326) # Convert back to wgs84
  return(geom_wgs84)
}

### PC Data - Extract Venice Blvd
# Load shp of all PCs, extract Venice Blvd
pc <- st_read('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/data/eval_extent/pc_05232017_wgs84_.shp')
venice_pc <- pc[27,]
veniceblvd <- st_read('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/data/eval_extent/tsp-extent_line.shp')
# Buffers
venice_pc_buff <- geom_buff(venice_pc, 100)
veniceblvd_buff <- geom_buff(veniceblvd, 100)

### HTML data cleaning
# Extract html data
html_data <- read_html('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/RA62/mymap14.html')

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

### Create Polylines
if(nrow(sf_df_filtered) > 1){
  
  # Merge points into linestring
  lafd_path <- sf_df_filtered %>%
    summarize(minTime = min(Timestamp),
              maxTime = max(Timestamp),
              Time = (max(Timestamp)-min(Timestamp)),
              do_union=FALSE) %>%
    st_cast("LINESTRING")
}

### Create the Map
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(popup = as.character(sf_df_filtered$Timestamp),
             data = sf_df_filtered
             ) %>%
  addPolylines(
    color = '#0E016F',
    weight = 3,
    opacity = 1,
    data = veniceblvd_buff # buffer geography by 50ft & clip
  ) %>%
  addPolylines(
    color = '#fc0307',
    weight = 3,
    opacity = 1,
    data = lafd_path # buffer geography by 50ft & clip
  )
m  # Print the map
