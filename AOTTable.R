library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


AOTTable <- function(id) {
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
    
  ))
  
}

AOTTableServer <- function(input, output, session) {
  
}