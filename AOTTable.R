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
        c(
          "CO",
          "SO2",
          "NO2",
          "Ozone",
          "PM10",
          "PM2.5",
          "Temp",
          "Light intensity",
          "Humidity"
        ),
        selected = TRUE
      )
    ),
    fluidRow(
      box(
        title = "SO2",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("so2"))
      ),
      box(
        title = "H2S",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("h2s"))
      ),
      box(
        title = "O3",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("o3"))
      ),
      box(
        title = "NO2",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("no2"))
      ),
      box(
        title = "CO",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("co"))
      ),
      box(
        title = "PM2.5",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("pm2_5"))
      ),
      box(
        title = "PM10",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("pm10"))
      ),
      box(
        title = "temperature",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("temperature"))
      ),
      box(
        title = "Light Intensity",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("intensity"))
      ),
      box(
        title = "Humidity",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        tableOutput(nameSpace("humidity"))
      )
    )
    
  ))
  
}

AOTTableServer <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(60000) # one minute
  
  observe({
    # timer runs out...
    autoInvalidate()
  })
  
  # ui ----
  
  output$so2 <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "so2")
    temperature
  })
  
  output$h2s <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "h2s")
    temperature
  })
  
  output$o3 <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "o3")
    temperature
  })
  
  output$no2 <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "no2")
    temperature
  })
  
  output$pm2_5 <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "pm2_5")
    temperature
  })
  
  output$pm10 <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "pm10")
    temperature
  })
  
  output$co <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "co")
    temperature
  })
  
  output$temp <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "temperature")
    temperature
  })
  
  output$intensity <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "intensity")
    temperature
  })
  
  output$humidity <- renderTable({
    autoInvalidate()
    full_data <- getAOTData()
    temperature <- getAOTvalue(full_data, "humidity")
    temperature
  })
  
}
