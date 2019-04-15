library('revgeo')
library(AotClient)
library('tidyverse')

all_node_data <- ls.nodes()

all_sensors <- ls.sensors()$path
so2_sensors <- all_sensors[grep("so2", all_sensors)]
h2s_sensors <- all_sensors[grep("h2s", all_sensors)]
o3_sensors <- all_sensors[grep("o3", all_sensors)]
no2_sensors <- all_sensors[grep("no2", all_sensors)]
co_sensors <- all_sensors[grep("co", all_sensors)]
pm2_5_sensors <- all_sensors[grep("pm2_5", all_sensors)]
pm10_sensors <- all_sensors[grep("pm10", all_sensors)]
temperature_sensors <- all_sensors[grep("temperature", all_sensors)]
intensity_sensors <- all_sensors[grep("light_intensity", all_sensors)]
humidity_sensors <- all_sensors[grep("humidity", all_sensors)]

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

getTimeFromToday <- function(period) {
  currTime = as.POSIXlt(Sys.time(), "UTC")
  
  if("day" %in% period){
    currTime = as.Date(currTime) - 1
  }
  else if("week" %in% period){
    currTime = as.Date(currTime) - 7
  }
  else{
    currTime = as.Date(currTime)
  }
  
  currTime = strftime(currTime, tz="UTC", format="lt:%Y-%m-%dT%H:%M:%S")
  return (currTime)
}

getAOTvalue <- function(period, value) {
  time = getTimeFromToday(period)
  
  sensor_list = c()
  if(value == "so2") {sensor_list <- so2_sensors}
  else if(value == "h2s") {sensor_list <- h2s_sensors}
  else if(value == "o3") {sensor_list <- o3_sensors}
  else if(value == "no2") {sensor_list <- no2_sensors}
  else if(value == "co") {sensor_list <- co_sensors}
  else if(value == "pm2_5") {sensor_list <- pm2_5_sensors}
  else if(value == "pm10") {sensor_list <- pm10_sensors}
  else if(value == "temperature") {sensor_list <- temperature_sensors}
  else if(value == "intensity") {sensor_list <- intensity_sensors}
  else if(value == "humidity") {sensor_list <- humidity_sensors}
  
  # TODO: only gets observations for one sensor on list, not all
  all_observations <- ls.observations(filters = list(sensor=sensor_list[1], timestamp=time))
  
  df <-
    data.frame(
      timestamp = all_observations$timestamp,
      value = all_observations$value,
      uom = all_observations$uom,
      sensor_path = all_observations$sensor_path,
      node_vsn = all_observations$node_vsn
    )
  
  df$address <- apply(df, 1, getAddressOfCurrNode)
  
  return (df)
}