### stplanr package to route using LA Street Network ###
library(stplanr)
library(sf)
library(dplyr)

# For example of routing in R, see 
# https://www.rdocumentation.org/packages/stplanr/versions/0.2.2/topics/SpatialLinesNetwork

# Import street centerline file & format
la_streets <- st_read("C:/Users/Tim/Downloads/Streets_Centerline/Streets_Centerline.shp",quiet=TRUE)
la_streets <- la_streets %>%
  dplyr::select(ASSETID, STNUM, ID, STNAME, OLD_STREET, geometry) %>%
  dplyr::mutate(len = st_length(geometry)) %>%
  filter(OLD_STREET %in% c('Secondary Highway',
                           'Local Street',
                           'Collector Street',
                           'Modified Collector Street',
                           'Major Highway - Class II',
                           'Modified Secondary Highway'))

# Convert to SpatialLines Network class
la_streets_SL <- as(la_streets, "Spatial")
la_streets_SLN <- SpatialLinesNetwork(la_streets_SL)

# Calculate shortest path, based on distance
shortpath <- sum_network_routes(la_streets_SLN, 300, 900, sumvars = "length")

# Plot the results
plot(shortpath, col = "red", lwd = 4)
plot(la_streets_SLN, add = TRUE)