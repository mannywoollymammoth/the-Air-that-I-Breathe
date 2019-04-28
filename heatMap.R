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



heatMap <- function(id) {
  nameSpace <- NS(id)
  fluidRow(column(12,
                  box(leafletOutput(
                    nameSpace("heatMap"), height = 700
                  ))),
           
           column(
             12,
             box(
               title = "Leaflet Map Parameters",
               solidHeader = TRUE,
               status = "primary",
               width = 12,
               radioButtons(
                 nameSpace("timeframe"),
                 inline = TRUE,
                 "Data to show:",
                 c("current",
                   "day",
                   "week"),
                 selected = c("current")
               )
             )
           ))
  
  # Define server logic required to draw a histogram
}


heatMapServer <- function(input, output, session) {
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
  
  
  AverageTemp <- function(x) {
    tryCatch({
      obs <-
        ls.observations(filters = list(node = x[3], sensor = 'metsense.bmp180.temperature'))
      average = aggregate(obs$value, by = list(obs$node_vsn), FUN = mean)
      average$longitude <- obs$location.geometry$coordinates[[1]][1]
      average$latitude <- obs$location.geometry$coordinates[[1]][2]
      #print(average)
      return(average)
      
    }, error = function(e) {
      
    })
  }
  
  output$heatMap <- renderLeaflet({
    Data <- getNodeTemps()
    
    nodeLocations <- getNodeGeoPoints()
    #Data$longitude <- nodeLocations$longitude
    #Data$latitude <- nodeLocations$latitude
    #print(Data)
    Data <-
      merge(
        x = Data,
        y = nodeLocations,
        by = "vsn",
        all.x = TRUE
      )
    #Data <- apply(nodeLocations, 1, AverageTemp)
    #Data <- do.call(rbind, Data)
    
    Data <- na.omit(Data)
    #print(Data)
    Data <-
      transform(Data,
                min = as.numeric(min),
                max = as.numeric(max),
                avg = as.numeric(avg))
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
    pal <- colorNumeric("Spectral", NULL, n = 13)
    
    leaflet(data = leafmap) %>%
      addTiles() %>% setView(lng = -87.647998,
                             lat = 41.870,
                             zoom = 12) %>%
      addPolygons(
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
    
  })
  
  
}
