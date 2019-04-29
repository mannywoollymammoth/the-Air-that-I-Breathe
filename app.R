# cs 424 - proj 3
# Fatima Qarni, Emmanuel Martinez

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

source('AOTMap.R')
source('AOTTable.R')

source('heatMap.R')
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Heat Map",
             icon = icon("th"),
             tabName = "heatMap"),
    menuItem(
      "AOT",
      tabName = "AOT",
      icon = icon("dashboard"),
      menuSubItem("AOTMap", tabName = "AOTMap"),
      menuSubItem("AOTTable", tabName = "AOTTable")
      
    )
    
  )),
  
  dynamicBody <- dashboardBody(tabItems(
    tabItem(tabName = "heatMap",
            heatMap(id="heatMap")),
    tabItem(tabName = "AOTMap",
            AOTMap(id = "AOTMap")),
    tabItem(tabName = "AOTTable",
            AOTTable(id = "AOTTable"))
  )),
  
  body <- dashboardBody(useShinyjs(),
                        dynamicBody)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(heatMapServer, id = "heatMap")
  callModule(AOTmapServer, id = "AOTMap")
  callModule(AOTTableServer, id = "AOTTable")
}

# Run the application
shinyApp(ui = ui, server = server)
