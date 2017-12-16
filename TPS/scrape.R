require(rvest)
library(magrittr)
library(stringr)
library(sf)

# Extract html data
html_data <- read_html('C:/Users/Tim/Documents/GitHub/Jonathans-Utilities/TPS/RA62/mymap0.html')

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
df$Timestamp <- strptime(df$Timestamp, format = "%d-%b-%y %H.%M.%S", "UTC")
df$Lat <- as.numeric(as.character(df$Lat))
df$Lng <- as.numeric(as.character(df$Lng))

# Convert to sf object
sf_df <- st_as_sf(df,
                  coords=c("Lat","Lng"),
                  remove=FALSE)

print(df)
