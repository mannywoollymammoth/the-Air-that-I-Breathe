library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
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
        width= 12,
        title = "Node 1 Data",
        id = "tabset2",
        height = 450,
        tabPanel(
          "Dark Sky Data",
          box(
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            dataTableOutput(nameSpace("darkSkyTableNode1"), width = "100%")
          )
        ),
        tabPanel(
          "AOT Data",
          box(
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            dataTableOutput(nameSpace("AOTTableNode1"), width = "100%")
          )
        )
      ), # Column for Node 2 data
      tabBox(
        title = "Node 2 Data",
        id = "tabset3",
        width = 12,
        height = 450,
        tabPanel(
          "Dark Sky Data",
          box(
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            dataTableOutput(nameSpace("darkSkyTableNode2"), width = "100%")
          )
        ),
        tabPanel(
          "AOT Data",
          box(
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            dataTableOutput(nameSpace("AOTTableNode2"), width = "100%")
          )
        )
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
               title = "Leaflet Map",
               width = 12,
               tabPanel("Tab1", leafletOutput(nameSpace("Normal"), height = 500)),
               tabPanel("Tab2", leafletOutput(nameSpace("StamenToner"), height = 500)),
               tabPanel("Tab3", leafletOutput(nameSpace("NightSky"), height = 500))
             )
       
  ), # end of column for node 1 data
  column(5,box(
    title = "Node Filter",
    solidHeader = TRUE,
    status = "primary",
    width = 12,
    radioButtons(
      nameSpace("nodeFilter"),
      inline = TRUE,
      "Data to show:",
      c("so2",
        "h2s",
        "o3",
        "no2",
        "co",
        "pm2_5",
        "pm10",
        "temperature",
        "intensity",
        "humidity"),
      selected = c("so2")
    )
  )
  ) # end of column for node 1 data 
  ) # this is where the fluid row for the two node tables ends
  )
  
}

# ============================================================= Pollutants

coList <- function(nodeLocations){
  coList <- ls.observations(filters=list(sensor = 'chemsense.co.concentration'))
  coNodeList <- unique(coList$node_vsn)
  
  coList <- nodeLocations %>% filter(vsn %in% coNodeList)
  print(coList)
  #co2List <- co2List %>% distinct(node_vsn, .keep_all = TRUE)
  #print(co2List)
  
  return(as.data.frame(coList))
}

no2List <- function(nodeLocations){
  no2List <- ls.observations(filters=list(sensor = 'chemsense.no2.concentration'))
  no2NodeList <- unique(no2List$node_vsn)
  no2List <- nodeLocations %>% filter(vsn %in% no2NodeList)
  return(as.data.frame(no2List))
}

h2sList <- function(nodeLocations){
  h2sList <- ls.observations(filters=list(sensor = 'chemsense.h2s.concentration'))
  h2sNodeList <- unique(h2sList$node_vsn)
  h2sList <- nodeLocations %>% filter(vsn %in% h2sNodeList)
  return(as.data.frame(h2sList))
}

o3List <- function(nodeLocations){
  o3List <- ls.observations(filters=list(sensor = 'chemsense.o3.concentration'))
  o3NodeList <- unique(o3List$node_vsn)
  o3List <- nodeLocations %>% filter(vsn %in% o3NodeList)
  return(as.data.frame(o3List))
}

so2List <- function(nodeLocations){
  so2List <- ls.observations(filters=list(sensor = 'chemsense.so2.concentration'))
  so2NodeList <- unique(so2List$node_vsn)
  so2List <- nodeLocations %>% filter(vsn %in% so2NodeList)
  return(as.data.frame(so2List))
}

pm10List <- function(nodeLocations){
  pm10List <- ls.observations(filters=list(sensor = 'alphasense.opc_n2.pm10'))
  pm10NodeList <- unique(pm10List$node_vsn)
  pm10List <- nodeLocations %>% filter(vsn %in% pm10NodeList)
  return(as.data.frame(pm10List))
}

pm2_5List <- function(nodeLocations){
  pm2_5List <- ls.observations(filters=list(sensor = 'alphasense.opc_n2.pm2_5'))
  pm2_5ListNodeList <- unique(pm2_5List$node_vsn)
  pm2_5List <- nodeLocations %>% filter(vsn %in% pm2_5ListNodeList)
  return(as.data.frame(pm2_5List))
}

lightList <- function(nodeLocations){
  lightList <- ls.observations(filters=list(sensor = 'lightsense.tsl250rd.intensity'))
  lightListNodeList <- unique(lightList$node_vsn)
  lightList <- nodeLocations %>% filter(vsn %in% lightListNodeList)
  return(as.data.frame(lightList))
}

tempList <- function(nodeLocations){
  tempList <- ls.observations(filters=list(sensor = 'metsense.tmp112.temperature'))
  tempListNodeList <- unique(tempList$node_vsn)
  tempList <- nodeLocations %>% filter(vsn %in% tempListNodeList)
  return(as.data.frame(tempList))
}

humList <- function(nodeLocations){
  humList <- ls.observations(filters=list(sensor = 'metsense.htu21d.humidity'))
  humListNodeList <- unique(humList$node_vsn)
  humList <- nodeLocations %>% filter(vsn %in% humListNodeList)
  return(as.data.frame(humList))
}

# ============================================================ Server starts here

AOTmapServer <- function(input, output, session) {
  dataSelectedReactive <- reactive(input$data_selected)
  ds_dataSelectedReactive <- reactive(input$ds_data_selected)
  node_filterReactive <- reactive(input$nodeFilter)
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
  
  # ==================================================== Node stuff
  
  nodeLocations <- read_csv('nodeLocations.csv')
  coList <- coList(nodeLocations)
  no2List <- no2List(nodeLocations)
  h2sList <- h2sList(nodeLocations)
  o3List <- o3List(nodeLocations)
  so2List <- so2List(nodeLocations)
  pm10List <- pm10List(nodeLocations)
  pm2_5List <- pm2_5List(nodeLocations)
  lightList <- lightList(nodeLocations)
  tempList <- tempList(nodeLocations)
  humList <- humList(nodeLocations)
  
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
      getNodeAOTData("current", reactiveValues$secondNode,
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
    
    
    if ("so2" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = so2List$longitude,
        lat = so2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = so2List$vsn,
        layerId = so2List$vsn
      )
    }
    else if ("h2s" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = h2sList$longitude,
        lat = h2sList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = h2sList$vsn,
        layerId = h2sList$vsn
      )
    }
    else if ("o3" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = o3List$longitude,
        lat = o3List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = o3List$vsn,
        layerId = o3List$vsn
      )
    }
    else if ("no2" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = no2List$longitude,
        lat = no2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = no2List$vsn,
        layerId = no2List$vsn
      )
    }
    else if ("co" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = coList$longitude,
        lat = coList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = coList$vsn,
        layerId = coList$vsn
      )
    }
    
    else if ("intensity" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = lightList$longitude,
        lat = lightList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = lightList$vsn,
        layerId = lightList$vsn
      )
    }
    
    else if ("temperature" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = tempList$longitude,
        lat = tempList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = tempList$vsn,
        layerId = tempList$vsn
      )
    }
    
    else if ("pm10" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = pm10List$longitude,
        lat = pm10List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm10List$vsn,
        layerId = pm10List$vsn
      )
    }
    
    else if ("pm2_5" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = pm2_5List$longitude,
        lat = pm2_5List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm2_5List$vsn,
        layerId = pm2_5List$vsn
      )
    }
    
    else if ("humidity" == node_filterReactive()){
      map %>% addCircleMarkers(
        lng = humList$longitude,
        lat = humList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = humList$vsn,
        layerId = humList$vsn
      )
    }
    
    
  })
  
  output$StamenToner <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    

    
    if ("so2" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = so2List$longitude,
        lat = so2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = so2List$vsn,
        layerId = so2List$vsn
      )
    }
    else if ("h2s" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = h2sList$longitude,
        lat = h2sList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = h2sList$vsn,
        layerId = h2sList$vsn
      )
    }
    else if ("o3" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = o3List$longitude,
        lat = o3List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = o3List$vsn,
        layerId = o3List$vsn
      )
    }
    else if ("no2" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = no2List$longitude,
        lat = no2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = no2List$vsn,
        layerId = no2List$vsn
      )
    }
    else if ("co" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = coList$longitude,
        lat = coList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = coList$vsn,
        layerId = coList$vsn
      )
    }
    
    else if ("intensity" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = lightList$longitude,
        lat = lightList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = lightList$vsn,
        layerId = lightList$vsn
      )
    }
    
    else if ("temperature" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = tempList$longitude,
        lat = tempList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = tempList$vsn,
        layerId = tempList$vsn
      )
    }
    
    else if ("pm10" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = pm10List$longitude,
        lat = pm10List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm10List$vsn,
        layerId = pm10List$vsn
      )
    }
    
    else if ("pm2_5" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = pm2_5List$longitude,
        lat = pm2_5List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm2_5List$vsn,
        layerId = pm2_5List$vsn
      )
    }
    
    else if ("humidity" == node_filterReactive()){
      map %>% addProviderTiles(providers$Stamen.Toner)  %>% addCircleMarkers(
        lng = humList$longitude,
        lat = humList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = humList$vsn,
        layerId = humList$vsn
      )
    }
    
    
    
    
  })
  
  output$NightSky <- renderLeaflet({
    coordinates <- getNodeGeoPoints()
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map,
                   lng = -87.647998,
                   lat = 41.870,
                   zoom = 12)
    
    
    if ("so2" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = so2List$longitude,
        lat = so2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = so2List$vsn,
        layerId = so2List$vsn
      )
    }
    else if ("h2s" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = h2sList$longitude,
        lat = h2sList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = h2sList$vsn,
        layerId = h2sList$vsn
      )
    }
    else if ("o3" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = o3List$longitude,
        lat = o3List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = o3List$vsn,
        layerId = o3List$vsn
      )
    }
    else if ("no2" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = no2List$longitude,
        lat = no2List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = no2List$vsn,
        layerId = no2List$vsn
      )
    }
    else if ("co" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = coList$longitude,
        lat = coList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = coList$vsn,
        layerId = coList$vsn
      )
    }
    
    else if ("intensity" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = lightList$longitude,
        lat = lightList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = lightList$vsn,
        layerId = lightList$vsn
      )
    }
    
    else if ("temperature" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = tempList$longitude,
        lat = tempList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = tempList$vsn,
        layerId = tempList$vsn
      )
    }
    
    else if ("pm10" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = pm10List$longitude,
        lat = pm10List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm10List$vsn,
        layerId = pm10List$vsn
      )
    }
    
    else if ("pm2_5" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = pm2_5List$longitude,
        lat = pm2_5List$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = pm2_5List$vsn,
        layerId = pm2_5List$vsn
      )
    }
    
    else if ("humidity" == node_filterReactive()){
      map %>% addProviderTiles(providers$Esri.WorldImagery)  %>% addCircleMarkers(
        lng = humList$longitude,
        lat = humList$latitude,
        radius = 4,
        color = "blue",
        fillOpacity = 1,
        popup = humList$vsn,
        layerId = humList$vsn
      )
    }
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
          color = node1Colors[1],
          size = 2
        ) + ylim(-15,20)
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
          color = node2Colors[1],
          size = 2
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
          color = node1Colors[2],
          size = 2
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
          color = node2Colors[2],
          size = 2
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
          color = node1Colors[3],
          size = 2
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
          color = node2Colors[3],
          size = 2
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
          color = node1Colors[4],
          size = 2
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
          color = node2Colors[4],
          size = 2
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
          color = node1Colors[5],
          size = 2
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
          color = node2Colors[5],
          size = 2
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
          color = node1Colors[6],
          size = 2
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
          color = node2Colors[6],
          size = 2
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
            color = node1Colors[6],
            size = 2
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
            color = node2Colors[6],
            size = 2
          )
      }
    }
    if ("humidity" %in% data_selected()) {
      if(node1Data$humidity[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node1Data,
            aes(
              y = node1Data$humidity,
              x = node1Data$timestamp,
              group = 1
            ),
            color = node1Colors[6],
            size = 2
          )+ ylim(-10, 120)
      }
      if(node2Data$humidity[1] != "N/A"){
        plot <-
          plot + geom_line(
            data = node2Data,
            aes(
              y = node2Data$humidity,
              x = node1Data$timestamp,
              group = 1
            ),
            color = node2Colors[6],
            size = 2
          )+ ylim(-10, 120)
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
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
            color = node1Colors[1],
            size = 2
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
            color = node2Colors[1],
            size = 2
          )
      }
    }
    
    plot <-  plot + theme_dark() +
      #scale_x_datetime(date_breaks = "1 hour") +
      theme(axis.text.x = element_text(angle = 50, vjust = 1.0, hjust = 1.0))
    plot
    
  })
}