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

source('Tables.R')

source('heatMap.R')
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Heat Map",
             icon = icon("th"),
             tabName = "heatMap"),
    menuItem(
      "Tables",
      icon = icon("th"),
      tabName = "Tables"
    )
  )),
  
  dynamicBody <- dashboardBody(tabItems(
    tabItem(tabName = "heatMap",
            heatMap(id="heatMap")),
    tabItem(tabName = "Tables",
            Tables(id = "Tables"))
  )),
  
  body <- dashboardBody(useShinyjs(),
                        dynamicBody)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(heatMapServer, id = "heatMap")
  callModule(TablesServer, id = "Tables")
}

# Run the application
shinyApp(ui = ui, server = server)
