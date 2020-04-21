
install.packages('shiny')
install.packages('leaflet')

library(shiny)
library(leaflet)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)

# ---------------------------------------------------------------------
# working buttons with leaflet map - Jackson 
  
ui <- fluidPage(
  titlePanel("Charlottesville Crime Map"),
  verticalLayout(sidebarPanel("OPTIONS", 
                              fluidRow(
     column(3, sliderInput("slider", "Time of Day", min = 0, max = 23, value = cData$hour)),
      column(3, checkboxGroupInput("gender", "Gender", c("Male", "Female"), select = "Male")),
      column(3, sliderInput("repeet", "Repeat Offenses", min= 1, max = 20, value = cData$NumOffenses)),
      column(3, checkboxGroupInput("violence", "Violence Level", c("1", "2", "3", "4", "5"))),
     column(3, checkboxGroupInput("race", "RACE", c("White", "Black", "Asian or Pacific Islander", "Not White"), selected= "White")),
     column(3, dateRangeInput("date", "DATE", start= "2019-01-01", end= "2020-04-16", format= "mm/dd/yy"))),
     width = 12),
  
  mainPanel("Crime Map", leafletOutput("mymap"))
 )
)
  
server <- function(input, output, session){
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = cData$longitude, lat= cData$latitude)
  })

  observe({
    
    hour <- input$hour
    offenses <- input$repeet
    gender <- input$gender
    date <- input$date
    violence <- input$violence
    race <- input$race
    
    newPoints <- cData %>%
      filter(cData$hour %in% hour & cData$NumOffenses %in% offenses & cData$Sex %in% gender & cData$VIOLENCE %in% violence & cData$Race %in% race)
    
    leafletProxy("mymap") %>% clearMarkers() %>%
      addMarkers(lng = newPoints$longitude, lat= newPoints$latitude)
  })

}

shinyApp(ui, server)








# --------------------------------------------------------------------


install.packages("DT")
library(DT)

Crime_Data
ArrestData_Cville_2019_pres_CLEAN

library(ggmap)

geocoded <- data.frame(stringsAsFactors = FALSE)

for (i in 1:nrow(ArrestData_Cville_2019_pres_CLEAN)){
  
  result <- geocode(ArrestData_Cville_2019_pres_CLEAN[i], output= "latlong", source= "google")
  ArrestData_Cville_2019_pres_CLEAN$lon[i] <- as.numeric(result[1])
  ArrestData_Cville_2019_pres_CLEAN$lat[i] <- as.numeric(result[2])
  ArrestData_Cville_2019_pres_CLEAN$geoAddress[i] <- as.character(result[3])
}

# ---------------------------------------------

# with popups 

  
ui <- fluidPage(
  titlePanel("Charlottesville Crime Map"),
  verticalLayout(sidebarPanel("OPTIONS", 
                              fluidRow(
     column(3, sliderInput("slider", "Time of Day", min = 0, max = 23, value = cData$hour)),
      column(3, checkboxGroupInput("gender", "Gender", c("Male", "Female"), select = "Male")),
      column(3, sliderInput("repeet", "Repeat Offenses", min= 1, max = 20, value = cData$NumOffenses)),
      column(3, checkboxGroupInput("violence", "Violence Level", c("1", "2", "3", "4", "5"))),
     column(3, checkboxGroupInput("race", "RACE", c("White", "Black", "Asian or Pacific Islander", "Not White"), selected= "White")),
     column(3, dateRangeInput("date", "DATE", start= "2019-01-01", end= "2020-04-16", format= "mm/dd/yy"))),
     width = 12),
  
  mainPanel("Crime Map", leafletOutput("mymap"))
 )
)
  
server <- function(input, output, session){
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = cData$longitude, lat= cData$latitude)
  })

  observe({
    
    hour <- input$hour
    offenses <- input$repeet
    gender <- input$gender
    date <- input$date
    violence <- input$violence
    race <- input$race
    
    newPoints <- cData %>%
      filter(cData$hour %in% hour & cData$NumOffenses %in% offenses & cData$Sex %in% gender & cData$VIOLENCE %in% violence & cData$Race %in% race)
    
    leafletProxy("mymap") %>% clearMarkers() %>%
      addMarkers(lng = newPoints$longitude, lat= newPoints$latitude, popup = paste("ArrestID", cData$ArrestID, "<br>",
                                                                                   "ArrestDate:", cData$ArrestDate, "<br>",
                                                                                   "Gender:", cData$Sex, "<br>",
                                                                                   "Race:", cData$Race))
  })

}

shinyApp(ui, server)


