library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(shinyjs)

source('DataModeler.R')


AOTTable <- function(id) {
  nameSpace <- NS(id)
  
  fluidRow(column(
    7,
    box(
      title = "Table Parameters",
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
        selected = c(
          "CO",
          "SO2",
          "NO2",
          "Ozone",
          "PM10",
          "PM2.5",
          "Temp",
          "Light intensity",
          "Humidity"
        )
      )
    ),
    box(
      title = "Leaflet Map Parameters",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      radioButtons(
        nameSpace("timeframe"),
        inline = TRUE,
        "Data to show:",
        c(
          "current",
          "day",
          "week"
        ),
        selected = c("current")
      )
    ),
    fluidRow(
      box(
        id = nameSpace("SO2"),
        title = "SO2",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("so2"))
      ),
      box(
        title = "H2S",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("h2s"))
      ),
      box(
        title = "O3",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("o3"))
      ),
      box(
        title = "NO2",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("no2"))
      ),
      box(
        title = "CO",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("co"))
      ),
      box(
        title = "PM2.5",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("pm2_5"))
      ),
      box(
        title = "PM10",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("pm10"))
      ),
      box(
        title = "temperature",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("temperature"))
      ),
      box(
        title = "Light Intensity",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("intensity"))
      ),
      box(
        title = "Humidity",
        solidHeader = TRUE,
        status = "primary",
        width = 8,
        dataTableOutput(nameSpace("humidity"))
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
  
  # TODO: I can't get this to work????? 
  observeEvent(input$data_selected,{
    
    nameSpace <- session$ns
    
    if("SO2" %in% input$data_selected) {
      print("test got here")
      show(id = "SO2")
    }
    else {
      hide(id = "SO2")
    }
    
    
    #hide(id = "SO2")
    # if(1) {
    #   #shinyjs::show(id = nameSpace("SO2"))
    #   hide(id = "so2")
    # }
    # else {
    #   shinyjs::hide(id = nameSpace("SO2"))
    # }
  })
  
  # ui ----
  
  output$so2 <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "so2")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$h2s <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "h2s")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$o3 <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "o3")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$no2 <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "no2")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$pm2_5 <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "pm2_5")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$pm10 <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "pm10")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$co <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "co")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$temp <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "temperature")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$intensity <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "intensity")
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$humidity <- renderDataTable({
    autoInvalidate()
    data <- getAOTvalue(input$timeframe, "humidity")
    
    datatable(data, options = list(pageLength = 5))
  })
  
}
