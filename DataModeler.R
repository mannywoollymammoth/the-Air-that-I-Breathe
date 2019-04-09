library('revgeo')
library(AotClient)
library('tidyverse')




getNodeGeoPoints <- function() {
  nodeLocations <- data.frame(ls.nodes(filters = list()))
  nodeLocations <- nodeLocations %>% select(location.geometry)
  coordinates <- nodeLocations$location.geometry$coordinates
  #print(typeof(coordinates[1]))
  #print(coordinates[[1]][1])
  latitude = c()
  longitude = c()
 
  for (point in coordinates){
    #print(point[1])
    if (point[1] != 0){
      longitude <- append(longitude, point[1], after = length(longitude))
      latitude <- append(latitude, point[2], after = length(latitude))
    }
    
  }
  #print(longitude)
  return (data.frame(longitude, latitude))
}
