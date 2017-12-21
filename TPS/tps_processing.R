library(dplyr)
library(readr)
library(leaflet)
library(sf)

### Read in Loop Data
loop_data <- read.csv('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/data/signal/WozSample.csv',
                      header = TRUE,
                      sep = ',',
                      stringsAsFactors = FALSE
                      )

### Dplyr formatting
# Exclude all error codes except 68111 & 68112
loop_data <- loop_data %>%
  # Convert timestamp to date/time value
  mutate(TIMESTMP = as.POSIXct(strptime(TIMESTMP,
                                        format = "%m/%d/%Y %H:%M",
                                        tz="America/Los_Angeles")),
         lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  # Only want "ISSUED" error codes (68111, 68112); also remove lat/lon NA values
  filter(ERRORCODE %in% c(68111,68112)
         ,!is.na(lat)
         ,!is.na(lon)
         )

# Convert df to sf object
loop_sf <- st_as_sf(loop_data,
                  coords=c("lon","lat"),
                  crs=4326,
                  remove=FALSE)

# Leaflet map w/ markers
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(popup = as.character(loop_sf$DET_ID),
             data = loop_sf
  )

# Print the map
m 
