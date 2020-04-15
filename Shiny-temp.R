library(shiny)
library(osmar)
library(prettymapr)
library(osmdata)
library(DBI)
library(RMySQL)
library(tidyverse)
library(anytime)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(zoo)
library(dplyr)
library(cluster)
library(sf)
library(ggfortify)
library(shiny)
library(leaflet)




arrests2 <- read.csv("C:/Users/student/Desktop/ArrestData_Cville_2019_WithCoordinates.csv")


# Define UI ----
ui <- fluidPage(
  titlePanel("Crime in Charlottesville"),
  
  verticalLayout(
                sidebarPanel("Options",
                             fluidRow(
                             column(3, 
                                    checkboxGroupInput("race", 
                                                       h3("Race"), 
                                                       choices = list("White" = 1, 
                                                                      "Black" = 2, 
                                                                      "Asian or Pacific Islander" = 3,
                                                                      "Not White" = 4),
                                                       selected = 1)),
                             
                             column(3,
                                    dateRangeInput("dates", h3("Date range"))),
                             
                             
                             column(3,
                                    selectInput("gender", h3("Select Gender"), 
                                                choices = list("All" = 1, "Male" = 2, "Female" = 3
                                                ), selected = 1)),
                             
                             column(3, 
                                    sliderInput("violence", h3("Violence Level"),
                                                min = 0, max = 100, value = 50)),
                             column(3,
                                    sliderInput("time", h3("Time of Day"),
                                                min = 0, max = 24, value = c(8, 16)))
                ), width = 12),
                mainPanel("Map",
                          leafletOutput(outputId = "map")))
  
  
  
  
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(arrests2) %>%
      setView(lng = -78.49,lat = 38.03, zoom = 2) %>%
      addTiles() %>%
      addCircles(data = arrests2, lat = ~ latitude, lng = ~ longitude)
    
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
