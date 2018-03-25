### stplanr package to route using LA Street Network ###
library(stplanr)
library(sf)
library(dplyr)
library(sp)

# For example of routing in R, see 
# https://www.rdocumentation.org/packages/stplanr/versions/0.2.2/topics/SpatialLinesNetwork

# Import street centerline file into sf dataframe object
la_streets <- st_read("C:/Users/Tim/Downloads/Streets_Centerline/Streets_Centerline.shp",quiet=TRUE)
la_streets <- la_streets %>%
  dplyr::select(ASSETID, STNUM, ID, STNAME, OLD_STREET, geometry) %>%
  dplyr::mutate(len = st_length(geometry)) %>%
  filter(OLD_STREET %in% c('Secondary Highway',
                           'Local Street',
                           'Collector Street',
                           'Modified Collector Street',
                           'Major Highway - Class II',
                           'Modified Secondary Highway')) %>%
  # coerce to linestring object from multi-linestring
  st_cast('LINESTRING')

# Method 1: Convert to Spatial* Object, then SpatialLinesNetwork Class
# Convert to SpatialLinesDataFrame
# 'as' method to coerce simple feature geometries to corresponding Spatial* objects (sp SpatialLinesDataFrame)
la_streets_SL <- as(la_streets, "Spatial")
# Convert to SpatialLines Network class 
# @nb = node values
la_streets_SLN <- SpatialLinesNetwork(la_streets_SL) 
# Identify nodes, correspond to SpatialLinesNetwork@nb values
la_streets_nodes <- sln2points(la_streets_SLN)

# Method 2: Convert to sfNetwork object (pure sf)
# As of 3/24/18, package doesn't appear to have support for sfNetwork class
# Should be able to take sf object, but cannot
#la_streets_sfN <- SpatialLinesNetwork(la_streets)


# Calculate shortest path, based on distance, using node IDs
shortpath <- sum_network_routes(la_streets_SLN, 300, 405, sumvars = "length")

# Plot the results, confirming that shortpath uses node IDs (not segment IDs) for routing
plot(shortpath, col = "red", lwd = 3)
plot(la_streets_SLN, col = "grey", add = TRUE)
plot(shortpath, col = "red", lwd = 3, add = TRUE)
plot(la_streets_nodes[c(300,405),], col = "black", pch = 16, cex = 1.2, asp = 1, add=TRUE)

# NEXT STEPS
# For each node ID in la_streets_SLN@nb, get object IDs of segments
# Join IDs to original data source, get list of unique names
# Concatenate list of unique street names to get an intersection name, eg 'Wilshire & Vermont'
# User searches, autocompletes intersection name to https://stackoverflow.com/questions/35265920/auto-complete-and-selection-of-multiple-values-in-text-box-shiny
# sum_network_routes runction converts to spatial object, then show on top of leaflet library.