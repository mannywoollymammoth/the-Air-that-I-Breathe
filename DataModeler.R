library('revgeo')
library(AotClient)
library('tidyverse')
library(darksky)

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
intensity_sensors <-
  all_sensors[grep("light_intensity", all_sensors)]
humidity_sensors <- all_sensors[grep("humidity", all_sensors)]

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
  
  return (data.frame(longitude, latitude, vsn))
}

getAddressOfCurrNode <- function(node) {
  node <- toString(node[5])
  
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
  newTime <- strftime(newTime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  return (newTime)
}

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


getNodeData <- function(node, values) {
  
  obs <- ls.observations(filters = list(sensor = o3_sensors[1], node = node))
  df <-
    data.frame(
      timestamp = obs$timestamp,
      uom = obs$uom,
      #sensor_path = obs$sensor_path,
      node_vsn = obs$node_vsn
    )
  
  parsed_sensor_list <- list()
  if ("so2" %in% values) {
    sensor_list <- so2_sensors
    so2 <- ls.observations(filters = list(sensor = 'chemsense.so2.concentration', node = node))
    df$so2Value = so2$value
    
  }
  if ("h2s" %in% values) {
    
    sensor_list <- h2s_sensors
    h2s <- ls.observations(filters = list(sensor = 'chemsense.h2s.concentration', node = node))
    df$h2sValue = h2s$value
  }
  
  if ("o3" %in% values) {
    sensor_list <- o3_sensors
    o3 <- ls.observations(filters = list(sensor = 'chemsense.o3.concentration' , node = node))
    df$o3Value = o3$value
  }
  
  if ("no2" %in% values) {
    sensor_list <- no2_sensors
    no2 <- ls.observations(filters = list(sensor ='chemsense.no2.concentration', node = node))
    df$no2Value = no2$value
  }
  if ("co" %in% values) {
    sensor_list <- co_sensors
    co <- ls.observations(filters = list(sensor = 'chemsense.co.concentration', node = node))
    df$coValue = co$value
  }
  if ("pm2_5" %in% values) {
    sensor_list <- pm2_5_sensors
    pm2_5 <- ls.observations(filters = list(sensor = sensor_list[1], node = node))
    df$pm2_5Value = pm2_5$value
  }
  if ("pm10" %in% values) {
    sensor_list <- pm10_sensors
    pm10 <- ls.observations(filters = list(sensor = sensor_list[1], node = node))
    df$pm10Value = pm10$value
  }
  if ("temperature" %in% values) {
    
    sensor_list <- temperature_sensors
  }
  if ("intensity" %in% values) {
    
    sensor_list <- intensity_sensors
  }
  if ("humidity" %in% values) {
    
    sensor_list <- humidity_sensors
  }
  
  # add address of current node
  df$address <- apply(df, 1, getAddressOfCurrNode)
  
  # change format of time column
  df$timestamp <- apply(df, 1, updateTimeFormatForPlot)
  
  return(df)
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
  
}

getNodeDarkSkyData <- function(period, lat, long) {
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
  } else {
    curr <- get_forecast_for(lat, long, time)$currently
    print(get_forecast_for(lat, long, time))
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
  
  df
  
}

getNodeOpenAQData <- function() {
  
}
