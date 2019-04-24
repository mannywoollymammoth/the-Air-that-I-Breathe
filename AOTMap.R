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
    ),
    box(
      title = "SO2",
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      plotOutput(nameSpace("so2graph"))
    ),
    box(
      title = "Node 1 Data",
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      dataTableOutput(nameSpace("nodeTable1"), width = "100%")
    ),
    box(
      title = "Node 2 Data",
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      dataTableOutput(nameSpace("nodeTable2"))
    )
    
    
  ),
  column(
    5,
    tabBox(
      title = "Leaflet Map",
      width = 12,
      tabPanel("Tab1", leafletOutput(nameSpace("Normal"), height = 600)),
      tabPanel("Tab2", leafletOutput(nameSpace("StamenToner"), height = 600)),
      tabPanel("Tab3", leafletOutput(nameSpace("NightSky"), height = 600))
    )
  ))
  
}

AOTmapServer <- function(input, output, session) {
  reactiveValues <- reactiveValues()
  reactiveValues$currNode <-
    "077"  # set a default val to start with
  reactiveValues$firstNode <- "077"
  reactiveValues$secondNode <- "004"
  reactiveValues$markerState <- 1
  autoInvalidate <- reactiveTimer(60000) # one minute
  
  observe({
    # timer runs out...
    autoInvalidate()
  })
  
  
  updateNodesWhenClicked <- function(currNodeId, mapType) {
    coordinates <- getNodeGeoPoints()
    currentPoint <-
      subset(coordinates, vsn == currNodeId)
    #subset depending on the first node and the second
    firstNode <-
      subset(coordinates, vsn == reactiveValues$firstNode)
    secondNode <-
      subset(coordinates, vsn == reactiveValues$secondNode)
    
    #print(currentPoint)
    if (is.null(click))
      return()
    else {
      if ((reactiveValues$markerState %% 2) != 0) {
        #we are keeping track of the two clicked nodes this way
        #keep a counter so we can mod it by two to see which node we change and which one will get set back
        #to the original blue color
        
        
        #change the current node to red
        leafletProxy(mapType) %>% removeMarker(reactiveValues$currNode) %>% addCircleMarkers(
          lng = currentPoint$longitude,
          lat = currentPoint$latitude,
          popup = currentPoint$vsn,
          layerId = currentPoint$vsn,
          radius = 4,
          color = "red",
          fillOpacity = 1
        )
        
        #make the previous node blue
        leafletProxy(mapType) %>% removeMarker(reactiveValues$firstNode) %>% addCircleMarkers(
          lng = firstNode$longitude,
          lat = firstNode$latitude,
          popup = firstNode$vsn,
          layerId = firstNode$vsn,
          radius = 4,
          color = "blue",
          fillOpacity = 1
        )
        #changing the node states
        reactiveValues$markerState <- reactiveValues$markerState + 1
        reactiveValues$firstNode <- reactiveValues$currNode
        print('first')
        
      }
      else if ((reactiveValues$markerState %% 2) == 0) {
        #we are keeping track of the two clicked nodes this way
        #keep a counter so we can mod it by two to see which node we change and which one will get set back
        #to the original blue color
        
        #change the current node to red
        leafletProxy(mapType) %>% removeMarker(reactiveValues$currNode) %>% addCircleMarkers(
          lng = currentPoint$longitude,
          lat = currentPoint$latitude,
          popup = currentPoint$vsn,
          layerId = currentPoint$vsn,
          radius = 4,
          color = "red",
          fillOpacity = 1
        )
        
        #make the previous node blue
        leafletProxy(mapType) %>% removeMarker(reactiveValues$secondNode) %>% addCircleMarkers(
          lng = secondNode$longitude,
          lat = secondNode$latitude,
          popup = secondNode$vsn,
          layerId = secondNode$vsn,
          radius = 4,
          color = "blue",
          fillOpacity = 1
        )
        #changing the node states
        reactiveValues$markerState <- reactiveValues$markerState + 1
        reactiveValues$secondNode <- reactiveValues$currNode
        print('second')
      }
    }
  }
  
  
  observeEvent(input$Normal_marker_click, {
    reactiveValues$currNode <- input$Normal_marker_click$id
    updateNodesWhenClicked(reactiveValues$currNode, "Normal")
  })
  observeEvent(input$StamenToner_marker_click, {
    reactiveValues$currNode <- input$StamenToner_marker_click$id
    updateNodesWhenClicked(reactiveValues$currNode, "StamenToner")
  })
  observeEvent(input$NightSky_marker_click, {
    reactiveValues$currNode <- input$NightSky_marker_click$id
    updateNodesWhenClicked(reactiveValues$currNode, "NightSky")
  })
  
  output$Normal <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
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
      fillOpacity = 1,
      popup = coordinates$vsn,
      layerId = coordinates$vsn
    )
  })
  
  
  output$StamenToner <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    
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
      fillOpacity = 1,
      popup = coordinates$vsn,
      layerId = coordinates$vsn
      
    )
  })
  
  output$NightSky <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    
    map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
      lng = coordinates$longitude,
      lat = coordinates$latitude,
      radius = 4,
      color = "blue",
      fillOpacity = 1,
      popup = coordinates$vsn,
      layerId = coordinates$vsn
    )
  })
  
  output$nodeTable1 <- renderDataTable({
    data <- node1DataReactive()
    datatable(data, options = list(pageLength = 5))
  })
  output$nodeTable2 <- renderDataTable({
    data <- node2DataReactive()
    datatable(data, options = list(pageLength = 5))
  })
  
  node1DataReactive <- reactive({
    tryCatch({
      getNodeData(reactiveValues$firstNode)
    },
    error = function(cond) {
      # TODO: lol this is a terrible way to write this code I'm sure but I couldn't figure it out
      janky_solution = ""
      validate(
        need(
          janky_solution != "",
          "No data available for this node. Please select a different node."
        )
      )
    })
  })
  
  node2DataReactive <- reactive({
    tryCatch({
      getNodeData(reactiveValues$secondNode)
    },
    error = function(cond) {
      # TODO: lol this is a terrible way to write this code I'm sure but I couldn't figure it out
      janky_solution = ""
      validate(
        need(
          janky_solution != "",
          "No data available for this node. Please select a different node."
        )
      )
    })
  })
  
  output$so2graph <- renderPlot({
    autoInvalidate()
    data <- node1DataReactive()
    print(data)
    ggplot(data, aes(y = data$value, x = data$timestamp)) + geom_point()
  })
  
  
}