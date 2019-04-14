library('revgeo')
library(AotClient)
library('tidyverse')

all_node_data <- ls.nodes()

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

getAddressOfCurrNode <- function(node) {
  node <- toString(node[5])
  
  address <- subset(all_node_data$address, all_node_data$vsn == node)
  address <- toString(address)
  
  return (address)
}

getAOTData <- function() {
  all_observations <- ls.observations()
  
  df <-
    data.frame(
      timestamp = all_observations$timestamp,
      value = all_observations$value,
      uom = all_observations$uom,
      sensor_path = all_observations$sensor_path,
      node_vsn = all_observations$node_vsn
    )
  
  df$address <- apply(df, 1, getAddressOfCurrNode)
  
  df
}

getAOTvalue <- function(all_data, value) {
  subset <- all_data[grep(value, all_data$sensor_path),]
  
  return (subset)
}