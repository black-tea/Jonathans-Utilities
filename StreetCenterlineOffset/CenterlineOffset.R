# load libraries
library(sf)
library(dplyr)
library(leaflet)
library(geosphere)

# import street data
bss <- st_read("Data/Street_Pavement_Condition.shp",quiet=TRUE)

# getting perpendicular points: see below:
# http://r-spatial.org/r/2017/11/13/perp-performance.html

# Get half of zwidth
bss$ZWIDTH2 <- bss$ZWIDTH/2


plot(bss)