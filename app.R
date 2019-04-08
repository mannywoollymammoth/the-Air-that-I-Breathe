#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

source('AOTMap.R')
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "AOTmap",
      tabName = "AOTMap",
      icon = icon("dashboard")
    ),
    menuItem(
      "Widgets",
      icon = icon("th"),
      tabName = "widgets",
      badgeLabel = "new",
      badgeColor = "green"
    )
  )),
  
  
  dynamicBody <- dashboardBody(tabItems(
    tabItem(
      tabName = "AOTMap",
      AOTMap(id = "AOTMap")
    ),
    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
    )
  )
  
  ),
  
  body <- dashboardBody(
    dynamicBody
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  callModule(AOTmapServer, id = "AOTMap")
}

# Run the application
shinyApp(ui = ui, server = server)
