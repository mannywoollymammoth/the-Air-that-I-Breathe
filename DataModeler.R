library('revgeo')
library(AotClient)
library('tidyverse')




getNodeGeoPoints <- function() {
  latitude = c()
  longitude = c()
  
  #query data from the AOT devices
  nodeLocations <- data.frame(ls.nodes(filters = list()))
  nodeLocations <- nodeLocations %>% select(location.geometry)
  coordinates <- nodeLocations$location.geometry$coordinates
  
  #remove any invalid coordinates from the list
  for (point in coordinates) {
    if (point[1] != 0) {
      longitude <- 
        append(longitude, point[1], after = length(longitude))
      latitude <-
        append(latitude, point[2], after = length(latitude))
    }
  }
  
  return (data.frame(longitude, latitude))
}
