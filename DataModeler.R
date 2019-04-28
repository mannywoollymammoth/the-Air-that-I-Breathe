library('revgeo')
library(AotClient)
library('tidyverse')
library(darksky)

all_node_data <- ls.nodes()

# list of all AOT sensors if you want to get data from all of them... probs not
all_sensors <- ls.sensors()$path

so2_sensors <- all_sensors[grep("so2", all_sensors)]
h2s_sensors <- all_sensors[grep("h2s", all_sensors)]
o3_sensors <- all_sensors[grep("o3", all_sensors)]
no2_sensors <- all_sensors[grep("no2", all_sensors)]
co_sensors <- all_sensors[grep("co", all_sensors)]
pm2_5_sensors <- all_sensors[grep("pm2_5", all_sensors)]
pm10_sensors <- all_sensors[grep("pm10", all_sensors)]
temperature_sensors <- all_sensors[grep("temperature", all_sensors)]
intensity_sensors <-
  all_sensors[grep("light_intensity", all_sensors)]
humidity_sensors <- all_sensors[grep("humidity", all_sensors)]

# list of one sensor for each AOT type
so2_sensor <- 'chemsense.so2.concentration'
h2s_sensor <- 'chemsense.h2s.concentration'
o3_sensor <- 'chemsense.o3.concentration'
no2_sensor <- 'chemsense.no2.concentration'
co_sensor <- 'chemsense.co.concentration'
pm2_5_sensor <- 'alphasense.opc_n2.pm2_5'
pm10_sensor <- 'alphasense.opc_n2.pm10 '
temperature_sensor <- 'chemsense.at0.temperature'
intensity_sensor <- 'chemsense.si1145.visible_light_intensity'
humidity_sensor <- 'metsense.htu21d.humidity'

# Set up for Dark Sky API
Sys.setenv(DARKSKY_API_KEY = scan("dark_sky_api_key.txt", what="character", sep=NULL) )

#returns list of latitudes and longitudes along with their VSN to identify them
getNodeGeoPoints <- function() {
  latitude = c()
  longitude = c()
  vsn = c()
  counter <- 1
  
  #query data from the AOT devices
  nodeLocations <- data.frame(ls.nodes(filters = list()))
  nodeLatLong <- nodeLocations %>% select(location.geometry)
  coordinates <- nodeLatLong$location.geometry$coordinates
  nodeID <- nodeLocations %>% select(vsn)
  nodeID <- as.character(nodeID$vsn)
  
  #remove any invalid coordinates from the list
  for (point in coordinates) {
    if (point[1] != 0) {
      longitude <-
        append(longitude, point[1], after = length(longitude))
      latitude <-
        append(latitude, point[2], after = length(latitude))
      vsn <-
        append(vsn, nodeID[counter], after = length(vsn))
    }
    counter <- counter + 1
  }
  #write the node locations to a file
  nodeLocations <- data.frame(longitude, latitude, vsn)
  write.csv(nodeLocations, file = "nodeLocations.csv")
  return (nodeLocations)
}

getAddressOfCurrNode <- function(node) {
  node <- toString(node[3])
  
  address <-
    subset(all_node_data$address, all_node_data$vsn == node)
  address <- toString(address)
  
  return (address)
}

getTimeFromToday <- function(period) {
  currTime = Sys.time()
  
  if ("day" == period) {
    currTime = as_datetime(currTime) - days(1)
  }
  else if ("week" == period) {
    currTime = as_datetime(currTime) - days(7)
  }
  else{
    currTime = as_datetime(currTime)
  }
  
  currTime = strftime(currTime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  return (currTime)
}

updateTimeFormatForPlot <- function(time) {
  time <- time[1]
  newTime <-
    strptime(toString(time), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  newTime <-strftime(newTime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  return (newTime)
}

# get a list of all the nodes with their addresses
# TODO: allow user to filter through the nodes based on what pollutants etc they want to see
# TODO: see if node is alive?
getAllNodeData <- function() {
  
  allData <- ls.nodes()
  df <- data.frame(
    node_num =allData$vsn, 
    address = allData$address
    )
  
  df
}


# returns data for all nodes, not just one specific node
# based on time period
getAOTvalue <- function(period, value) {
  time = getTimeFromToday(period)
  time = strftime(time, tz = "UTC", format = "lt:%Y-%m-%dT%H:%M:%S")
  
  sensor_list = c()
  if (value == "so2") {
    sensor_list <- so2_sensors
  }
  else if (value == "h2s") {
    sensor_list <- h2s_sensors
  }
  else if (value == "o3") {
    sensor_list <- o3_sensors
  }
  else if (value == "no2") {
    sensor_list <- no2_sensors
  }
  else if (value == "co") {
    sensor_list <- co_sensors
  }
  else if (value == "pm2_5") {
    sensor_list <- pm2_5_sensors
  }
  else if (value == "pm10") {
    sensor_list <- pm10_sensors
  }
  else if (value == "temperature") {
    sensor_list <- temperature_sensors
  }
  else if (value == "intensity") {
    sensor_list <- intensity_sensors
  }
  else if (value == "humidity") {
    sensor_list <- humidity_sensors
  }
  
  #df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("timestamp", "value", "uom", "sensor_path", "node_vsn"))
  
  #for (name in temperature_sensors) {
  #  curr <- ls.observations(filters = list(sensor=name, timestamp=time))
  #}
  
  # TODO: only gets observations for one sensor on list, not all
  all_observations <-
    ls.observations(filters = list(sensor = sensor_list[1], timestamp = time))
  
  df <-
    data.frame(
      timestamp = all_observations$timestamp,
      value = all_observations$value,
      uom = all_observations$uom,
      sensor_path = all_observations$sensor_path,
      node_vsn = all_observations$node_vsn
    )
  
  # add address of current node
  df$address <- apply(df, 1, getAddressOfCurrNode)
  
  # change format of time column
  df$timestamp <- apply(df, 1, updateTimeFormatForPlot)
  
  return (df)
}

# gets all data for one node, 
# based on @values - which contains the list of pollutants that are requested 
# & based on time period
getNodeAOTData <- function(period, node, values) {
  time = getTimeFromToday(period)
  time = strftime(time, tz = "UTC", format = "lt:%Y-%m-%dT%H:%M:%S")
  
  request_size = 200
  
  if(period == "week") {
    request_size = 10000  # this is cut down to ~200 later
  }

  obs <- ls.observations(filters = list(sensor = o3_sensors[1], size = request_size, node = node, timestamp = time))
  
  df <-
    data.frame(
      timestamp = obs$timestamp,
      uom = obs$uom,
      node_vsn = obs$node_vsn
    ) 
  
  if(period == "week") {
    df <-
      data.frame(
        timestamp = listShortenerByManny(obs$timestamp),
        uom = listShortenerByManny(obs$uom),
        node_vsn = listShortenerByManny(obs$node_vsn)
      )
  }

  if ("so2" %in% values) {
    df$so2 <- "N/A"
    try({
      so2 <- ls.observations(filters = list(sensor = so2_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        so2 <- listShortenerByManny(so2)
      }
      df$so2 = so2
    })
  }
  if ("h2s" %in% values) {
    df$h2s <- "N/A"
    try({
      h2s <- ls.observations(filters = list(sensor = h2s_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        h2s <- listShortenerByManny(h2s)
      }
      df$h2s = h2s
    })
  }
  if ("o3" %in% values) {
    df$o3 <- "N/A"
    try({
      o3 <- ls.observations(filters = list(sensor = o3_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        o3 <- listShortenerByManny(o3)
      }
      df$o3 = o3
    })
  }
  if ("no2" %in% values) {
    df$no2 <- "N/A"
    try({
      no2 <- ls.observations(filters = list(sensor = no2_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        no2 <- listShortenerByManny(no2)
      }
      df$no2 = no2
    })
  }
  if ("co" %in% values) {
    df$co <- "N/A"
    try({
      co <- ls.observations(filters = list(sensor = co_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        co <- listShortenerByManny(co)
      }
      df$co = co
    })
  }
  if ("pm2_5" %in% values) {
    df$pm2_5 <- "N/A"
    try({
      pm2_5 <- ls.observations(filters = list(sensor = pm2_5_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        pm2_5 <- listShortenerByManny(pm2_5)
      }
      df$pm2_5 = pm2_5
    })
  }
  if ("pm10" %in% values) {
    df$pm10 <- "N/A"
    try({
      pm10 <- ls.observations(filters = list(sensor = pm10_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        pm10 <- listShortenerByManny(pm10)
      }
      df$pm10 = pm10
    })
  }
  if ("temperature" %in% values) {
    df$temperature <- "N/A"
    try({
      temper <- ls.observations(filters = list(sensor = temperature_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        temper <- listShortenerByManny(temper)
      }
      df$temperature = temper
    })
  }
  if ("intensity" %in% values) {
    df$intensity <- "N/A"
    try({
      intensity <- ls.observations(filters = list(sensor = intensity_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        intensity <- listShortenerByManny(intensity)
      }
      df$intensity = intensity
    })
  }
  if ("humidity" %in% values) {
    df$humidity <- "N/A"
    try({
      humid <- ls.observations(filters = list(sensor = humidity_sensor, size = request_size, node = node, timestamp = time))$value
      if(period=="week") {
        humid <- listShortenerByManny(humid)
      }
      df$humidity = humid
    })
  }
  
  # change format of time column
  df$timestamp <- apply(df, 1, updateTimeFormatForPlot)
  
  return(df)
}

listShortenerByManny <- function(orig) {
  new =  orig[c(
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )]
  
  new
}

getNodeTemps <- function() {
  # returns a df of the min, max, and average value at each node
  
  # empty dataframe
  df <-
    setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("vsn", "min", "max", "avg"))
  time = getTimeFromToday("day")
  time = strftime(time, tz = "UTC", format = "lt:%Y-%m-%dT%H:%M:%S")
  vsns <- all_node_data$vsn
  
  for (num in vsns) {
    tryCatch({
      #TODO: I just chose a temp sensor that seemed more accurate here for more values... its still massively
      # off for some of them so we will have to figure out a better sensor to use? Or use all of them..?? idk
      vals = ls.observations(filters = list(
        sensor = "chemsense.at0.temperature",
        node = num,
        timestamp = time
      ))
      
      min <- min(vals$value)
      max <- max(vals$value)
      avg <- ave(vals$value)[1]
      
      # add each new row
      df[nrow(df) + 1,] <- c(num, min, max, avg)
    },
    error = function(cond) {
    })
  }
  return(df)
}

getNodeDarkSkyData <- function(period, lat, long, values) {
  # get data based on the time period requested and the lat/long of the current node
  time = getTimeFromToday(period)
  
  curr = NULL
  
  if (period=="week") {
    
    currReq <- get_forecast_for(lat, long, time)$hourly
    
    curr <- data.frame(
      time = currReq$time,
      summary = currReq$summary,
      temperature = currReq$temperature,
      humidity = currReq$humidity,
      windSpeed = currReq$windSpeed,
      windBearing = currReq$windBearing,
      cloudCover = currReq$cloudCover,
      visibility = currReq$visibility,
      pressure = currReq$pressure
    )
    
    for (i in 1:6) {
      time = as_datetime(time) + days(1)
      time = strftime(time, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
      
      curr2 <- get_forecast_for(lat, long, time)$hourly
      
      curr2 <- data.frame(
        time = curr2$time,
        summary = curr2$summary,
        temperature = curr2$temperature,
        humidity = curr2$humidity,
        windSpeed = curr2$windSpeed,
        windBearing = curr2$windBearing,
        cloudCover = curr2$cloudCover,
        visibility = curr2$visibility,
        pressure = curr2$pressure
      )
      
      curr <- rbind(curr, curr2)
      
    }
  } 
  else if (period=="day") {
    curr <- get_forecast_for(lat, long, time)$hourly
  } 
  else {
    curr <- get_forecast_for(lat, long, time)$currently
  }
  
  #TODO: on the website it says he wanted the ozone as well.. but that's not an option....?
  
  df <- data.frame(
    time = curr$time,
    summary = curr$summary,
    temperature = curr$temperature,
    humidity = curr$humidity,
    windSpeed = curr$windSpeed,
    windBearing = curr$windBearing,
    cloudCover = curr$cloudCover,
    visibility = curr$visibility,
    pressure = curr$pressure
  )
  
  # drop all columns that are not included in values
  drops <- numeric()
  
  if(!"temperature" %in% values){
    drops <- append(drops, "temperature")
  }
  if(!"humidity" %in% values){
    drops <- append(drops, "humidity")
  }
  if(!"wind speed" %in% values){
    drops <- append(drops, "windSpeed")
  }
  if(!"wind bearing" %in% values){
    drops <- append(drops, "windBearing")
  }
  if(!"cloud cover" %in% values){
    drops <- append(drops, "cloudCover")
  }
  if(!"visibility" %in% values){
    drops <- append(drops, "visibility")
  }
  if(!"pressure" %in% values){
    drops <- append(drops, "pressure")
  }
  
  if(length(drops) != 0) {
    df <- df[, !(names(df) %in% drops)]
  }
  
  df
}

getNodeOpenAQData <- function() {
  
}
