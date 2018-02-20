library(stplanr)

# Example source
# https://github.com/ropensci/stplanr/blob/master/R/SpatialLinesNetwork.R
# See lines 458 - 466

data(routes_fast)
rnet <- overline(routes_fast, attrib = "length")
SLN <- SpatialLinesNetwork(rnet)
weightfield(SLN) # field used to determine shortest path
shortpath <- sum_network_routes(SLN, 1, 17, sumvars = "length")
plot(shortpath, col = "red", lwd = 4)
plot(SLN, add = TRUE)