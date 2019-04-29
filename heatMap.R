library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(RColorBrewer)
library(rgdal)
library(AotClient)
library('tidyverse')
library(sp)
library(maptools)
library(maps)


source('DataModeler.R')
source('AOTMap.R')


heatMap <- function(id) {
  nameSpace <- NS(id)
  fluidRow(
    column(8,
           box(
             width = 12, leafletOutput(nameSpace("heatMap"), height = 700)
           )),
    column(3,
           box(
             title = "Data Bounds",
             solidHeader = TRUE,
             status = "primary",
             width = 6,
             radioButtons(
               nameSpace("dataBounds"),
               inline = TRUE,
               "Data to show:",
               c("min",
                 "max",
                 "avg"),
               selected = c("avg")
             )
           )
    ),
    
  fluidRow(
    box(
      title = "Dark Sky or AOT",
      solidHeader = TRUE,
      status = "primary",
      width = 6,
      radioButtons(
        nameSpace("DarkOrAOT"),
        inline = TRUE,
        "Data to show:",
        c("DarkSky",
          "AOT"),
        selected = c("DarkSky")
      )
    ),
    
    box(
      title = "Leaflet Time Parameters",
      solidHeader = TRUE,
      status = "primary",
      width = 6,
      radioButtons(
        nameSpace("timeframe"),
        inline = TRUE,
        "Data to show:",
        c("current",
          "day",
          "week"),
        selected = c("day")
      )
    ),
    box(
      title = "Leaflet Aot Environment Parameters",
      solidHeader = TRUE,
      status = "primary",
      width = 6,
      radioButtons(
        nameSpace("Environment"),
        inline = TRUE,
        "Data to show:",
        c(
          "so2",
          "h2s",
          "o3",
          "no2",
          "co",
          "pm2_5",
          "pm10",
          "intensity",
          "humidity",
          "temperature"
        ),
        selected = c("so2")
      )
    ),
    box(
      title = "Leaflet DarkSky Environment Parameters",
      solidHeader = TRUE,
      status = "primary",
      width = 6,
      radioButtons(
        nameSpace("DarkEnvironment"),
        inline = TRUE,
        "Data to show:",
        c(
          "temperature",
          "humidity",
          "wind speed",
          "wind bearing",
          "cloud cover",
          "visibility",
          "pressure"
        ),
        selected = c("temperature")
      )
    ))
  )
  
  # Define server logic required to draw a histogram
}



heatMapServer <- function(input, output, session) {
  reactiveValues <- reactiveValues()
  env_selected <- reactive(input$Environment)
  darkEnv_selected <- reactive(input$DarkEnvironment)
  dataSet_selected <- reactive(input$DarkOrAOT)
  time_selected <- reactive(input$timeframe)
  reactiveValues$env_selected <- NULL
  dataBounds_selected <- reactive(input$dataBounds)
  
  nodeLocations <- read_csv('nodeLocations.csv')
  coList <- coList(nodeLocations)
  no2List <- no2List(nodeLocations)
  h2sList <- h2sList(nodeLocations)
  o3List <- o3List(nodeLocations)
  so2List <- so2List(nodeLocations)
  pm10List <- pm10List(nodeLocations)
  pm2_5List <- pm2_5List(nodeLocations)
  lightList <- lightList(nodeLocations)
  tempList <- tempList(nodeLocations)
  humList <- humList(nodeLocations)
  
  #this function will go through the list of node locations and query dark sky
  #it should return a dataframe complete with the data that we need
  getDarkSkyData <- function(period, nodeLocations) {
    avg <- list()
    vsn <- list()
    for (row in 1:10) {
      df <-
        getNodeDarkSkyData(period,
                           nodeLocations$latitude[row],
                           nodeLocations$longitude[row],
                           darkEnv_selected())
      groupByList <- rep('tag', nrow(df))
      df$groupByList <- groupByList
      
      
      
      #here we will only aggregate what ever is selected int the radio box
      if ('temperature' == darkEnv_selected()) {
        average = aggregate(df$temperature,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      else if ('cloud cover' == darkEnv_selected()) {
        average = aggregate(df$cloudCover,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      else if ('visibility' == darkEnv_selected()) {
        average = aggregate(df$visibility,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      else if ('pressure' == darkEnv_selected()) {
        average = aggregate(df$pressure,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      
      else if ('wind bearing' == darkEnv_selected()) {
        average = aggregate(df$windBearing,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      
      else if ('wind speed' == darkEnv_selected()) {
        average = aggregate(df$windSpeed,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      
      else if ('humidity' == darkEnv_selected()) {
        average = aggregate(df$humidity,
                            by = list(df$groupByList),
                            FUN = mean)
      }
      
      avg <- append(avg, average$x)
      vsn <-  append(vsn, nodeLocations$vsn[row])
    }
    avg <- do.call(rbind, avg)
    vsn <- do.call(rbind, vsn)
    
    return(data.frame(vsn, avg))
  }
  
  getAOTData <- function()
  {
    if('co' == env_selected()){
      Data <- getNodeData2(coList, 'chemsense.co.concentration')
    }
    else if('h2s' == env_selected()){
      Data <- getNodeData2(h2sList, 'chemsense.h2s.concentration')
    }
    else if('o3' == env_selected()){
      Data <- getNodeData2(o3List, 'chemsense.o3.concentration')
    }
    else if('so2' == env_selected()){
      Data <- getNodeData2(so2List, 'chemsense.so2.concentration')
    }
    else if('intensity' == env_selected()){
      Data <- getNodeData2(so2List, 'lightsense.tsl250rd.intensity')
    }
    else if('humidity' == env_selected()){
      Data <- getNodeData2(humList, 'metsense.htu21d.humidity')
    }
    else if('pm2_5' == env_selected()){
      Data <- getNodeData2(pm2_5List, 'alphasense.opc_n2.pm2_5')
    }
    else if('pm10' == env_selected()){
      Data <- getNodeData2(pm10List, 'alphasense.opc_n2.pm10')
    }
    else if('temperature' == env_selected()){
      Data <- getNodeTemps2(tempList)
      print(Data)
    }
    
    Data <-
      transform(
        Data,
        min = as.numeric(min),
        max = as.numeric(max),
        avg = as.numeric(avg)
      )
    return(Data)
  }
  
  
  output$heatMap <- renderLeaflet({
    nodeLocations <- read_csv('nodeLocations.csv')
    reactiveValues$env_selected <- env_selected()
    
    
    if ('DarkSky' == dataSet_selected()) {
      Data <- getDarkSkyData(time_selected(), nodeLocations)
    }
    else{
      
      #print(DataTest)
      #Data <- getNodeTemps2(tempList)
      
      
      # Data <-
      #   transform(
      #     Data,
      #     min = as.numeric(min),
      #     max = as.numeric(max),
      #     avg = as.numeric(avg)
      #   )
      Data <- getAOTData()
      #print(Data)
    }
    
    Data <-
      merge(
        x = Data,
        y = nodeLocations,
        by = "vsn",
        all.x = TRUE
      )
    
    
    
    Data <- na.omit(Data)
    #print(Data)
    
    #for the node temp we need this
    #
    coordinates(Data) = ~ longitude + latitude
    proj4string(Data) <-
      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    chicago.map.hoods <-
      readOGR(dsn = './Boundaries',
              layer = "geo_export_31991cc3-ec20-448e-90f4-b537d46bc5a0",
              stringsAsFactors = FALSE)
    
    
    proj4string(chicago.map.hoods) <-
      CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    dataWithTag <- over(chicago.map.hoods, Data)
    dataWithTag <-
      mutate(dataWithTag, area_num_1 = rownames(dataWithTag))
    dataWithTag <- na.omit(dataWithTag)
    
    
    leafmap <-
      merge(chicago.map.hoods, dataWithTag, by = 'area_num_1')
    pal <- colorNumeric("YlOrRd", NULL, n = 13)
    
    map <- leaflet(data = leafmap) %>%
      addTiles() %>% setView(lng = -87.647998,
                             lat = 41.870,
                             zoom = 11)
      
    
    if ('avg' == dataBounds_selected()){
      map <- map %>% addPolygons(
        fillColor = ~ pal(avg),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1
      ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~ avg,
          opacity = 1
        )
    }
    
    else if ('min' == dataBounds_selected()){
      map <- map %>% addPolygons(
        fillColor = ~ pal(min),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1
      ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~ min,
          opacity = 1
        )
    }
    
    else if ('max' == dataBounds_selected()){
      map <- map %>% addPolygons(
        fillColor = ~ pal(max),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1
      ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~ max,
          opacity = 1
        )
    }
    
  })
  
  
}
