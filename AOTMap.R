library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

source('DataModeler.R')


AOTMap <- function(id) {
  nameSpace <- NS(id)
  
  fluidRow(column(
    7,
    box(
      title = "Leaflet Map Parameters",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      checkboxGroupInput(
        nameSpace("data_selected"),
        inline = TRUE,
        "Data to show:",
        c("CO", "SO2", "NO2", "Ozone", "PM10", "PM2.5", "Wind", "Temp"),
        selected = TRUE
      )
      
    )
    
    
  ),
  column(
    5,
    
    tabBox(
      title = "Leaflet Map",
      width = 12,
      tabPanel("Tab1", leafletOutput(nameSpace("Normal"), height = 600)),
      tabPanel("Tab2", leafletOutput(nameSpace("StamenTonerMap"), height = 600)),
      tabPanel("Tab3", leafletOutput(nameSpace("NightSky"), height = 600))
    )
    
    
  ))
  
}

AOTmapServer <- function(input, output, session) {
  output$Normal <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    print("longitude")
    print(coordinates[1])
    print("latitude")
    print(coordinates[2])
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    
    #map %>% addMarkers(lng = coordinates$longitude, lat = coordinates$latitude)
    map %>% addCircleMarkers(
      lng = coordinates$longitude,
      lat = coordinates$latitude,
      radius = 4,
      color = "blue",
      fillOpacity = 1
    )
  })
  
  observeEvent(input$Normal_marker_click, { 
    node <- input$Normal_marker_click
    print(node)
  })
  
  
  output$StamenTonerMap <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    print("longitude")
    print(coordinates[1])
    print("latitude")
    print(coordinates[2])
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    
    #map %>% addMarkers(lng = coordinates$longitude, lat = coordinates$latitude)
    map %>% addProviderTiles(providers$Stamen.Toner) %>% addCircleMarkers(
      lng = coordinates$longitude,
      lat = coordinates$latitude,
      radius = 4,
      color = "blue",
      fillOpacity = 1
    )
    
    
  })
  
  
  output$NightSky <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    print("longitude")
    print(coordinates[1])
    print("latitude")
    print(coordinates[2])
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    
    #map %>% addMarkers(lng = coordinates$longitude, lat = coordinates$latitude)
    map %>% addProviderTiles(providers$Esri.WorldImagery) %>% addCircleMarkers(
      lng = coordinates$longitude,
      lat = coordinates$latitude,
      radius = 4,
      color = "blue",
      fillOpacity = 1
    )
    
    
  })
  
}