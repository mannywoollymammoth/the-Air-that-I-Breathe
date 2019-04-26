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
        c( "so2", "h2s", "o3", "no2", "co", "pm2_5", "pm10", "temperature", "intensity", "humidity"),
        selected = TRUE
      )
    ),
    box(
      title = "Current",
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      plotOutput(nameSpace("lineGraphCurrent"))
    ),
    box(
      title = "7 Days",
      solidHeader = TRUE,
      status = "primary",
      width = 8,
      plotOutput(nameSpace("lineGraph7Days"))
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
  dataSelectedReactive <- reactive(input$data_selected)
  reactiveValues <- reactiveValues()
  
  # set a default val to start with
  reactiveValues$currNode <-"077" 
  reactiveValues$firstNode <- "077"
  reactiveValues$secondNode <- "004"
  reactiveValues$markerState <- 1
  data_selected <- reactive(input$data_selected)
  reactiveValues$data_selected <- NULL
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
        #print('first')
        
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
        #print('second')
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
    data <- node1CurrentDataReactive()
    datatable(data, options = list(pageLength = 5))
  })
  output$nodeTable2 <- renderDataTable({
    data <- node2CurrentDataReactive()
    datatable(data, options = list(pageLength = 5))
  })
  
  node1CurrentDataReactive <- reactive({
    tryCatch({
      #print("getting to getnodeData")
      #print(reactiveValues$data_selected)
      getNodeData(reactiveValues$firstNode, reactiveValues$data_selected)
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
  
  node2CurrentDataReactive <- reactive({
    tryCatch({
      #print("second node here")
      #print(reactiveValues$secondNode)
      getNodeData(reactiveValues$secondNode, reactiveValues$data_selected)
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
  
  node1weekDataReactive <- reactive({
    tryCatch({
      #print("getting to getnodeData")
      #print(reactiveValues$data_selected)
      getNodeData7Days(reactiveValues$firstNode, reactiveValues$data_selected)
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
  
  node2weekDataReactive <- reactive({
    tryCatch({
      #print("second node here")
      #print(reactiveValues$secondNode)
      getNodeData7Days(reactiveValues$secondNode, reactiveValues$data_selected)
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
  
  output$lineGraphCurrent <- renderPlot({
    autoInvalidate()
    reactiveValues$data_selected <- data_selected()
    node1Data <- node1CurrentDataReactive()
    print(node1Data)
    node2Data <- node2CurrentDataReactive()
    
    node1Colors <-brewer.pal(n=6,name = 'OrRd')
    node2Colors <-brewer.pal(n=6,name = 'BuPu')
    
    plot <- ggplot() + ylim(-10, 20)
    
    
    
    
    if ("so2" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$so2Value, x = node1Data$timestamp, group = 1), color = node1Colors[1]) 
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$so2Value, x = node1Data$timestamp, group = 1), color = node2Colors[1]) 
    }
    if ("h2s" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$h2sValue, x = node1Data$timestamp, group = 1), color = node1Colors[2])
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$h2sValue, x = node1Data$timestamp, group = 1), color = node2Colors[2]) 
    }
    if ("o3" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$o3Value, x = node1Data$timestamp, group = 1), color = node1Colors[3])
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$o3Value, x = node1Data$timestamp, group = 1), color = node2Colors[3]) 
    }
    if ("no2" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$no2Value, x = node1Data$timestamp, group = 1), color = node1Colors[4])
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$no2Value, x = node1Data$timestamp, group = 1), color = node2Colors[4]) 
    }
    if ("co" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$coValue, x = node1Data$timestamp, group = 1), color = node1Colors[5])
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$coValue, x = node1Data$timestamp, group = 1), color = node2Colors[5]) 
    }
    if ("pm2_5" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$pm2_5Value, x = node1Data$timestamp, group = 1), color = node1Colors[6])
      plot <- plot + geom_line(data = node2Data, aes(y = node2Data$pm2_5Value, x = node1Data$timestamp, group = 1), color = node2Colors[6]) 
    }
    if ("pm10" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$pm10Value, x = node1Data$timestamp, group = 1))
    }
    plot <-  plot + theme_dark()
    plot
    
    
    
  })
  
  
  
  
  
  updateTimeFormatForPlot <- function(time, sensor) {
    time <- time[1]
    newTime <-
      strptime(toString(time), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    newTime <- strftime(newTime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    return (newTime)
  }
  
  getData7Day <- function(node){
    currDay <- Sys.Date()
    time <- Sys.time()
    currTime <- strftime(now, format="%H:%M:%S")
    weekTimeStamp <- paste("lt:", currDay,"T", currTime, sep = "")
    
    Data <- ls.observations(filters = list(size = 10000, sensor = 'chemsense.so2.concentration', node = node, timestamp = weekTimeStamp))
    df <- data.frame(
      timestamp = Data$timestamp,
      uom = Data$uom,
      node_vsn = Data$node_vsn,
      value <- Data$value
    )
    
    Data <- df$value[c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
    TimeStamp <- df$timestamp[c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
    
    df <- data.frame(timestamp <- TimeStamp, value <- Data)
    df$timestamp <- apply(df, 1, updateTimeFormatForPlot)
    return(df)
  }
  
  
  output$lineGraph7Days <- renderPlot({
    autoInvalidate()
    reactiveValues$data_selected <- data_selected()
    node1Colors <-brewer.pal(n=6,name = 'OrRd')
    node2Colors <-brewer.pal(n=6,name = 'BuPu')

    plot <- ggplot() + ylim(-10, 20)


    if ("so2" %in% data_selected()) {
      
      node1df <- getData7Day(reactiveValues$firstNode)
      node2df <- getData7Day(reactiveValues$secondNode)
      
      
      plot <- plot + geom_line(data = node1df, aes(y = node1df$value, x = node1df$timestamp, group = 1), color = node1Colors[1])
      plot <- plot + geom_line(data = node2df, aes(y = node2df$value, x = node2df$timestamp, group = 1), color = node2Colors[1])
    }
    if ("h2s" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$h2sValue, x = node1Data$timestamp, group = 1), color = node1Colors[2])
      plot <- plot + geom_line(data = node1Data, aes(y = node2Data$h2sValue, x = node1Data$timestamp, group = 1), color = node2Colors[2])
    }
    if ("o3" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$o3Value, x = node1Data$timestamp, group = 1), color = node1Colors[3])
      plot <- plot + geom_line(data = node1Data, aes(y = node2Data$o3Value, x = node1Data$timestamp, group = 1), color = node2Colors[3])
    }
    if ("no2" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$no2Value, x = node1Data$timestamp, group = 1), color = node1Colors[4])
      plot <- plot + geom_line(data = node1Data, aes(y = node2Data$no2Value, x = node1Data$timestamp, group = 1), color = node2Colors[4])
    }
    if ("co" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$coValue, x = node1Data$timestamp, group = 1), color = node1Colors[5])
      plot <- plot + geom_line(data = node1Data, aes(y = node2Data$coValue, x = node1Data$timestamp, group = 1), color = node2Colors[5])
    }
    if ("pm2_5" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$pm2_5Value, x = node1Data$timestamp, group = 1), color = node1Colors[6])
      plot <- plot + geom_line(data = node1Data, aes(y = node2Data$pm2_5Value, x = node1Data$timestamp, group = 1), color = node2Colors[6])
    }
    if ("pm10" %in% data_selected()) {
      plot <- plot + geom_line(data = node1Data, aes(y = node1Data$pm10Value, x = node1Data$timestamp, group = 1))
    }
    plot <-  plot + theme_dark()
    plot



  })
  
  
}