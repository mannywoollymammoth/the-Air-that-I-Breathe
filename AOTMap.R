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
  fluidRow( # This is the main fluid row
    
    fluidRow(column( #graph column starts
      7,
      fluidRow(
        box(
          title = "AOT Data Graphs",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          plotOutput(nameSpace("AOTLineGraph"))
        )
      ),
      fluidRow(
        box(
          title = "Dark Sky Data Graphs",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          plotOutput(nameSpace("DarkSkyLineGraph"))
        )
      )
    ), # graph column ends
    
    column( # map column starts
      5,
      tabBox(
        title = "Leaflet Map",
        width = 12,
        tabPanel("Tab1", leafletOutput(nameSpace("Normal"), height = 500)),
        tabPanel("Tab2", leafletOutput(nameSpace("StamenToner"), height = 500)),
        tabPanel("Tab3", leafletOutput(nameSpace("NightSky"), height = 500))
      )
    ) # map column ends
    ),
    
    fluidRow( # This is the fluid row for the 2 node tables + options
      column(1, # options
             fluidRow(
               box(
                 title = "AOT/OpenAQ Parameters",
                 solidHeader = TRUE,
                 status = "primary",
                 width = 12,
                 checkboxGroupInput(
                   nameSpace("data_selected"),
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
                   selected = TRUE
                 )
               )
             ),
             fluidRow(
               box(
                 title = "Dark Sky Parameters",
                 solidHeader = TRUE,
                 status = "primary",
                 width = 12,
                 checkboxGroupInput(
                   nameSpace("ds_data_selected"),
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
                   selected = TRUE
                 )
               )
             ),
             fluidRow(
               box(
                 title = "Timeframe for Data Tables",
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
             )
             ), 
      column(1,
               box(
                 title = "Timeframe for Data Tables",
                 solidHeader = TRUE,
                 status = "primary",
                 width = 12,
                 dataTableOutput(nameSpace("AOTAllNodesTable"), width = "100%")
               )
             ), # end of options
      column(5, # Column for Node 1 data
        tabBox(
          width= 12,
          title = "Node 1 Data",
          id = "tabset2",
          height = 550,
          tabPanel(
            "AOT Data",
            box(
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              dataTableOutput(nameSpace("AOTTableNode1"), width = "100%")
            )
          ),
          tabPanel(
            "Dark Sky Data",
            box(
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              dataTableOutput(nameSpace("darkSkyTableNode1"), width = "100%")
            )
          )
        )
  ), # end of column for node 1 data
  column(5, # Column for Node 2 data
    tabBox(
      title = "Node 2 Data",
      id = "tabset3",
      height = 550,
      tabPanel(
        "AOT Data",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          dataTableOutput(nameSpace("AOTTableNode2"), width = "100%")
        )
      ),
      tabPanel(
        "Dark Sky Data",
        box(
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          dataTableOutput(nameSpace("darkSkyTableNode2"), width = "100%")
        )
      )
    )
  ) # end of column for node 1 data 
  ) # this is where the fluid row for the two node tables ends
  )
  
}

AOTmapServer <- function(input, output, session) {
  dataSelectedReactive <- reactive(input$data_selected)
  ds_dataSelectedReactive <- reactive(input$ds_data_selected)
  reactiveValues <- reactiveValues()
  
  # set a default val to start with
  reactiveValues$currNode <- "077"
  
  reactiveValues$firstNode <- "077"
  reactiveValues$secondNode <- "004"
  
  reactiveValues$currNodeLat <- 41.83107
  reactiveValues$currNodeLong <- -87.6173
  
  reactiveValues$curr1NodeLat <- 41.83107
  reactiveValues$curr1NodeLong <- -87.6173
  
  reactiveValues$curr2NodeLat <- 41.87838
  reactiveValues$curr2NodeLong <- -87.62768
  
  reactiveValues$markerState <- 1
  
  data_selected <- reactive(input$data_selected)
  ds_data_selected <- reactive(input$ds_data_selected)
  reactiveValues$data_selected <- NULL
  reactiveValues$ds_data_selected <- NULL
  
  autoInvalidate <- reactiveTimer(60000) # one minute
  
  # ===================================================== AOT
  
  node1AOTCurrentDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("current", reactiveValues$firstNode,
                     reactiveValues$data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2AOTCurrentDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("current", reactiveValues$firstNode,
                     reactiveValues$data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node1AOTDayDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("day", reactiveValues$firstNode,
                  reactiveValues$data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2AOTDayDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("day", reactiveValues$secondNode,
                  reactiveValues$data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node1AOTWeekDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("week", 
                         reactiveValues$firstNode,
                       reactiveValues$data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2AOTWeekDataReactive <- reactive({
    tryCatch({
      getNodeAOTData("week", 
                         reactiveValues$secondNode,
                       reactiveValues$data_selected)
    },
    error = function(cond) {
      #print(cond)
      showErrorMessageForNoNodeData()
    })
  })
  
  # ====================================================== DARK SKY
  
  node1DarkSkyCurrentDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("current",
                         reactiveValues$curr1NodeLat,
                         reactiveValues$curr1NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2DarkSkyCurrentDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("current",
                         reactiveValues$curr1NodeLat,
                         reactiveValues$curr1NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node1DarkSkyDayDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("day",
                         reactiveValues$curr1NodeLat,
                         reactiveValues$curr1NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2DarkSkyDayDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("day",
                         reactiveValues$curr2NodeLat,
                         reactiveValues$curr2NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node1DarkSkyWeekDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("week",
                         reactiveValues$curr1NodeLat,
                         reactiveValues$curr1NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  node2DarkSkyWeekDataReactive <- reactive({
    tryCatch({
      getNodeDarkSkyData("week",
                         reactiveValues$curr1NodeLat,
                         reactiveValues$curr1NodeLong,
                         reactiveValues$ds_data_selected)
    },
    error = function(cond) {
      showErrorMessageForNoNodeData()
    })
  })
  
  # ============================================ observe/update nodes
  
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
        
        reactiveValues$curr1NodeLat <- reactiveValues$currNodeLat
        reactiveValues$curr1NodeLong <- reactiveValues$currNodeLong
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
        
        reactiveValues$curr2NodeLat <- reactiveValues$currNodeLat
        reactiveValues$curr2NodeLong <- reactiveValues$currNodeLong
      }
    }
  }
  
  observeEvent(input$Normal_marker_click, {
    reactiveValues$currNode <- input$Normal_marker_click$id
    reactiveValues$currNodeLat <- input$Normal_marker_click$lat
    reactiveValues$currNodeLong <- input$Normal_marker_click$lng
    
    updateNodesWhenClicked(reactiveValues$currNode, "Normal")
  })
  observeEvent(input$StamenToner_marker_click, {
    reactiveValues$currNode <- input$StamenToner_marker_click$id
    #TODO: add lat long here
    updateNodesWhenClicked(reactiveValues$currNode, "StamenToner")
  })
  observeEvent(input$NightSky_marker_click, {
    reactiveValues$currNode <- input$NightSky_marker_click$id
    #TODO: add lat long here
    updateNodesWhenClicked(reactiveValues$currNode, "NightSky")
  })
  
  showErrorMessageForNoNodeData <- function() {
    # TODO: lol this is a terrible way to write this code I'm sure but I couldn't figure it out
    janky_solution = ""
    validate(
      need(
        janky_solution != "",
        "No data available for this node. Please select a different node."
      )
    )
  }
  
  # ============================================================ helpers
  
  updateTimeFormat <- function(time) {
    newTime <- as.POSIXct(strptime(newTime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
    return (newTime)
  }
  
  # ============================================================ UI - Maps
  
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
  
  # ============================================================ UI - Tables
  
  output$AOTAllNodesTable <- renderDataTable({
    data <- getAllNodeData()
    datatable(data, options = list(pageLength = 5))
  })
  
  output$AOTTableNode1 <- renderDataTable({
    autoInvalidate()
    data <- NULL
    
    if (input$timeframe == "current") {
      data <- node1AOTCurrentDataReactive()
    } else if (input$timeframe == "day") {
      data <- node1AOTDayDataReactive()
    } else if (input$timeframe == "week") {
      data <- node1AOTWeekDataReactive()
    }
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$AOTTableNode2 <- renderDataTable({
    autoInvalidate()
    data <- NULL
    
    if (input$timeframe == "current") {
      data <- node2AOTCurrentDataReactive()
    } else if (input$timeframe == "day") {
      data <- node2AOTDayDataReactive()
    } else if (input$timeframe == "week") {
      data <- node2AOTWeekDataReactive()
    }
    
    datatable(data, options = list(pageLength = 5))
  })
  
  output$darkSkyTableNode1 <- renderDataTable({
    autoInvalidate()
    reactiveValues$ds_data_selected <- ds_data_selected()
    data <- NULL
    
    if (input$timeframe == "current") {
      data <- node1DarkSkyCurrentDataReactive()
    } else if (input$timeframe == "day") {
      data <- node1DarkSkyDayDataReactive()
    } else if (input$timeframe == "week") {
      data <- node1DarkSkyWeekDataReactive()
    }
    datatable(data, options = list(pageLength = 5))
  })
  
  output$darkSkyTableNode2 <- renderDataTable({
    autoInvalidate()
    reactiveValues$ds_data_selected <- ds_data_selected()
    data <- NULL
    
    if (input$timeframe == "current") {
      data <- node2DarkSkyCurrentDataReactive()
    } else if (input$timeframe == "day") {
      data <- node2DarkSkyDayDataReactive()
    } else if (input$timeframe == "week") {
      data <- node2DarkSkyWeekDataReactive()
    }
    datatable(data, options = list(pageLength = 5))
  })
  
  # ============================================================ UI - Graphs
  
  output$AOTLineGraph <- renderPlot({
    autoInvalidate()
    reactiveValues$data_selected <- data_selected()
    
    # choose node data based on the selected value for the time period
    if (input$timeframe == "current") {
      node1Data <- node1AOTCurrentDataReactive()
      node2Data <- node2AOTCurrentDataReactive()
    } else if (input$timeframe == "day") {
      node1Data <- node1AOTDayDataReactive()
      node2Data <- node2AOTDayDataReactive()
    } else if (input$timeframe == "week") {
      node1Data <- node1AOTWeekDataReactive()
      node2Data <- node2AOTWeekDataReactive()
    }
    
    # convert timestamp to posixct
    #node1Data$timestamp <- apply(df, 1, updateTimeFormat)
    #node2Data$timestamp <- apply(df, 1, updateTimeFormat)
    
    node1Colors <- brewer.pal(n = 6, name = 'OrRd')
    node2Colors <- brewer.pal(n = 6, name = 'BuPu')
    
    plot <- ggplot() + ylim(-10, 20)
    
    if ("so2" %in% data_selected()) {
      if(node1Data$so2[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$so2,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[1]
        )
      }
      if(node2Data$so2[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$so2,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[1]
        )
      }
    }
    if ("h2s" %in% data_selected()) {
      if(node1Data$h2s[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$h2s,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[2]
        )
      }
      if(node2Data$h2s[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$h2s,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[2]
        )
      }
    }
    if ("o3" %in% data_selected()) {
      if(node1Data$o3[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$o3,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[3]
        )
      }
      if(node2Data$o3[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$o3,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[3]
        )
      }
    }
    if ("no2" %in% data_selected()) {
      if(node1Data$no2[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$no2,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[4]
        )
      }
      if(node2Data$no2[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$no2,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[4]
        )
      }
    }
    if ("co" %in% data_selected()) {
      if(node1Data$co[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$co,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[5]
        )
      }
      if(node2Data$co[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$co,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[5]
        )
      }
    }
    if ("pm2_5" %in% data_selected()) {
      if(node1Data$pm2_5[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node1Data,
          aes(
            y = node1Data$pm2_5,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node1Colors[6]
        )
      }
      if(node2Data$pm2_5[1] != "N/A"){
      plot <-
        plot + geom_line(
          data = node2Data,
          aes(
            y = node2Data$pm2_5,
            x = node1Data$timestamp,
            group = 1
          ),
          color = node2Colors[6]
        )
      }
    }
    if ("pm10" %in% data_selected()) {
      if(node1Data$pm10[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$pm10,
              x = node1Data$timestamp,
              group = 1
            ),
            color = node1Colors[6]
          )
      }
      if(node2Data$pm10[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$pm10,
              x = node1Data$timestamp,
              group = 1
            ),
            color = node2Colors[6]
          )
      }
    }
    plot <-  plot + theme_dark() +
      #scale_x_datetime(date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0))
    plot
  })
  
  output$DarkSkyLineGraph <- renderPlot({
    autoInvalidate()
    reactiveValues$data_selected <- ds_data_selected()
    
    # choose node data based on the selected value for the time period
    if (input$timeframe == "current") {
      node1Data <- node1DarkSkyCurrentDataReactive()
      node2Data <- node2DarkSkyCurrentDataReactive()
    } else if (input$timeframe == "day") {
      node1Data <- node1DarkSkyDayDataReactive()
      node2Data <- node2DarkSkyDayDataReactive()
    } else if (input$timeframe == "week") {
      node1Data <- node1DarkSkyWeekDataReactive()
      node2Data <- node2DarkSkyWeekDataReactive()
    }
    
    node1Colors <- brewer.pal(n = 6, name = 'OrRd')
    node2Colors <- brewer.pal(n = 6, name = 'BuPu')
    
    plot <- ggplot() + ylim(-10, 20)
    
    if ("temperature" %in% ds_data_selected()) {
      if(node1Data$temperature[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$temperature,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$temperature[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$temperature,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("humidity" %in% ds_data_selected()) {
      if(node1Data$humidity[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$humidity,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$humidity[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$humidity,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("wind speed" %in% ds_data_selected()) {
      if(node1Data$windSpeed[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$windSpeed,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$windSpeed[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$windSpeed,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("wind bearing" %in% ds_data_selected()) {
      if(node1Data$windBearing[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$windBearing,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$windBearing[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$windBearing,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("cloud cover" %in% ds_data_selected()) {
      if(node1Data$cloudCover[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$cloudCover,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$cloudCover[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$cloudCover,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("visibility" %in% ds_data_selected()) {
      if(node1Data$visibility[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$visibility,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$visibility[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$visibility,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    if ("pressure" %in% ds_data_selected()) {
      if(node1Data$pressure[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$pressure,
              x = node1Data$time,
              group = 1
            ),
            color = node1Colors[1]
          )
      }
      if(node2Data$pressure[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$pressure,
              x = node2Data$time,
              group = 1
            ),
            color = node2Colors[1]
          )
      }
    }
    
    plot <-  plot + theme_dark() +
      #scale_x_datetime(date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0))
    plot
    
  })
}