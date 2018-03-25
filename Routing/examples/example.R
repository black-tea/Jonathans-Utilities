library(stplanr)

# Example source
# https://github.com/ropensci/stplanr/blob/master/R/SpatialLinesNetwork.R
# See lines 458 - 466
# @section Details:
# Find the shortest path on the network between specified nodes and returns
# a SpatialLinesdataFrame containing the path(s) and summary statistics of
# each one.
#
# @param sln The SpatialLinesNetwork to use.
# @param start Node ID(s) of route starts.
# @param end Node ID(s) of route ends.
# @param sumvars Character vector of variables for which to calculate
# summary statistics.
# @param combinations Boolean value indicating if all combinations of start
# and ends should be calculated. If TRUE then every start Node ID will be routed
# to every end Node ID. This is faster than passing every combination to start
# and end. Default is FALSE.
#
# @examples
# data(routes_fast)
# rnet <- overline(routes_fast, attrib = "length")
# SLN <- SpatialLinesNetwork(rnet)
# weightfield(SLN) # field used to determine shortest path
# shortpath <- sum_network_routes(SLN, 1, 50, sumvars = "length")
# plot(shortpath, col = "red", lwd = 4)
# plot(SLN, add = TRUE)
# @export

#data(routes_fast)
#rnet <- overline(routes_fast, attrib = "length")
#SLN <- SpatialLinesNetwork(rnet)
#weightfield(SLN) # field used to determine shortest path
#shortpath <- sum_network_routes(SLN, 1, 17, sumvars = "length")
#plot(shortpath, col = "red", lwd = 4)
#plot(SLN, add = TRUE)

# Example source for sum_network_links function
# Summarise links from shortest paths data
# See lines 622-648
# @section Details:
# Find the shortest p-ath on the network between specified nodes and returns
# a SpatialLinesDataFrame or sf containing the path(s) and summary statistics
# of each one.
#
# @param sln The SpatialLinesNetwork or sfNetwork to use.
# @param routedata A dataframe where the first column contains the Node ID(s)
# of the start of the routes, the second column indicates the Node ID(s) of
# the end of the routes, and any additional columns are summarised by link.
# If there are no additional colums, then overlapping routes are counted.
# @examples
# data(routes_fast)
# rnet <- overline(routes_fast, attrib = "length")
# SLN <- SpatialLinesNetwork(rnet)
# weightfield(SLN) # field used to determine shortest path
# shortpath <- sum_network_links(
#     SLN,
#     data.frame(
#         start=rep(c(1,2,3,4,5),each=4),
#         end=rep(c(50,51,52,33),times=5)
#     )
# )
# plot(shortpath, lwd=shortpath$count)
#
# @export
# https://github.com/ropensci/stplanr/blob/master/R/SpatialLinesNetwork.R
# See lines 458 - 466
SLN_sf <- SpatialLinesNetwork(route_network_sf)
plot(SLN_sf)
