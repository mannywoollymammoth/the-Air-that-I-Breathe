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
  fluidRow(column(8,
                  box(
                    width = 12, leafletOutput(nameSpace("heatMap"), height = 700)
                  )),
           
           column(
             4,
             box(
               title = "Leaflet Time Parameters",
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
             ),
             box(
               title = "Leaflet Pollutant Parameters",
               solidHeader = TRUE,
               status = "primary",
               width = 12,
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
                   "temperature",
                   "intensity",
                   "humidity"
                 ),
                 selected = c("temperature")
               )
             )
           ))
  
  # Define server logic required to draw a histogram
}


heatMapServer <- function(input, output, session) {
  reactiveValues <- reactiveValues()
  env_selected <- reactive(input$Environment)
  reactiveValues$env_selected <- NULL
  
  
  
  #this function will go through the list of node locations and query dark sky
  #it should return a dataframe complete with the data that we need
  getDarkSkyData <- function(period, nodeLocations){
    avg <- list()
    vsn <- list()
    for(row in 1:30){
      
      df <- getNodeDarkSkyData('day',nodeLocations$latitude[row], nodeLocations$longitude[row], 'temperature')
      groupByList <- rep('tag', nrow(df))
      df$groupByList <- groupByList
      average = aggregate(df$temperature,by=list(df$groupByList),  FUN = mean)
      avg <- append(avg, average$x)
      vsn <-  append(vsn, nodeLocations$vsn[row])
    }
    avg <- do.call(rbind, avg)
    vsn <- do.call(rbind, vsn)
    
    print(avg)
    print(vsn)
    #df2 <- getNodeDarkSkyData('week', -87.71054, 41.86635)
    return(data.frame(vsn,avg))
  }
  
  
  output$heatMap <- renderLeaflet({
    nodeLocations <- read_csv('nodeLocations.csv')
    Data <- getDarkSkyData('test', nodeLocations)
    
    #Data <- getNodeTemps()
    #print(Data)
    reactiveValues$env_selected <- env_selected()
    
    #nodeLocations <- getNodeGeoPoints()
    #read in as tibble and still worked woot
    
   
    Data <-
      merge(
        x = Data,
        y = nodeLocations,
        by = "vsn",
        all.x = TRUE
      )
  
    #print(Data)
    if ("so2" == env_selected()) {
     
    }
    if("temperature" == env_selected()){
      
    }
    

    
    Data <- na.omit(Data)
    #print(Data)
    
    #for the node temp we need this
    # Data <-
    #    transform(Data,
    #              min = as.numeric(min),
    #              max = as.numeric(max),
    #              avg = as.numeric(avg))
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
                             zoom = 11) %>%
      addPolygons(
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
    
  })
  
  
}
