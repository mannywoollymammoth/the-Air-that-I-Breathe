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
  dashboardHeader(title = "CS 424 - Project 3 - Fatima & Manny",
                  
                  tags$li(
                    actionLink("openInfo", label = " Project Info", icon = icon("info")),
                    class = "dropdown"
                  )),
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
  
  observeEvent(input$openInfo, {
    showModal(modalDialog(
      title = "Fatima Qarni & Emmanuel Martinez",
      p(
        "Libraries used for visualization: shiny, shinydashboard, ggplot2, lubridate, DT, jpeg, grid, leaflet, scales, dplyr, RColorBrewer, shinyjs, AotClient, tidyverse, darksky. \n
        AoT data comes from: https://aot-file-browser.plenar.io/data-sets/chicago-complete
        AoT API: https://github.com/UrbanCCD-UChicago/aot-client-r
        Dark Sky API: https://github.com/hrbrmstr/darksky

        References: https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile

        https://gis.stackexchange.com/questions/137621/join-spatial-point-data-to-polygons-in-r
        
        https://rstudio-pubs-static.s3.amazonaws.com/212793_f130ecc723da4ed98680d6d3d5c4aff9.html
        
        http://ryanruthart.com/using-r-to-perform-a-spatial-join-and-aggregate-data/
        
        https://franciscorequena.com/blog/how-to-make-an-interactive-map-of-usa-with-r-and-leaflet/"
      )
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
