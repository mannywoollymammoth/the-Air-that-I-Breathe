library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


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
    box(
      title = "Leaflet Map",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      leafletOutput(nameSpace("leaf"), height = 600)
    )
    
  ))
  
}

AOTmapServer <- function(input, output, session) {
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    map <-
      addMarkers(map,
                 lng = -87.6477,
                 lat = 41.8698,
                 popup = "evl")
    map
  })
}