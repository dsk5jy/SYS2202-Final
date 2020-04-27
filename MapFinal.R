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
library(cluster)
library(smoother)
library(factoextra)
#------------- Import arrest data and fuck w the columns----------
arrests2 <- read_excel("Downloads/ArrestData_Cville_2019-pres_CLEAN (1).xlsx")
arrests2$fulladdress<-arrests2$FULL_ADDRESS
arrests2$FULL_ADDRESS<-NULL
arrests2$NameSuffix<-NULL
arrests2<-separate(arrests2, TIME, into=c("Date", "Time"), sep= " ")
arrests2$Date<-NULL
arrests2<-separate(arrests2, Time, into=c("hour", "minute", "second"), sep=":")
arrests2$MINUTE<-NULL
#--- get geo coordinates from address---------
geocoded2<-data.frame(arrests2)
register_google(key="AIzaSyBtSvnbnZzZdQ3K93rjCkW56mY6iXKC0XA")
has_google_key()
for(i in 1:nrow(arrests2)){
  result<-geocode(arrests2$fulladdress[i], output="latlona", source="google")
  arrests2$longitude[i]<-as.numeric(result[1])
  arrests2$latitude[i]<-as.numeric(result[2])
}
arrests2<-na.omit(arrests2)
ArrestData_Cville_2019_WithCoordinates<-arrests2
# ------------------------------------------------------------
cData <- ArrestData_Cville_2019_WithCoordinates
cData$hour <- as.numeric(cData$hour)
cData$ArrestDate <- as.Date(cData$ArrestDate)

latlong <- data.frame(longitude = cData$longitude, latitude = cData$latitude)
latlong <- filter(latlong, latlong$longitude <= -78.45 & latlong$longitude >= -78.525 & latlong$latitude <= 38.1 & latlong$latitude >= 38)

ggplot(cData) + geom_point(aes(x = longitude, y = latitude, color = VIOLENCE)) + xlim(-78.525, -78.45) + ylim(38, 38.1) + ggtitle("Initital Location Plot")

kman <- kmeans(latlong, centers = 4, nstart = 25)
map4 <- fviz_cluster(kman, data = latlong)

map <- ggplot(cData) + geom_point(aes(x = longitude, y = latitude))

cData$ArrestDate <- as.Date(cData$ArrestDate, format = "MM/DD/YY")

cData$violencelev <- ifelse(cData$VIOLENCE <=2, "mild",
                               ifelse(cData$VIOLENCE >2 | cData$VIOLENCE <5,"medium",
                                      ifelse(cData$VIOLENCE >= 5, "Bad")))
# -------------------------------------------

ui <- fluidPage(
  titlePanel("Charlottesville Crime Map"),
  verticalLayout(sidebarPanel("OPTIONS", 
                              fluidRow(
                                column(3, sliderInput("slider", "Time of Day", min = 0, max = 23, value = cData$hour)),
                                column(3, checkboxGroupInput("gender", "Gender", c("Male", "Female"), select = "Male")),
                                column(3, sliderInput("repeet", "Repeat Offenses", min= 1, max = 20, value = cData$NumOffenses)),
                                column(3, checkboxGroupInput("violence", "Violence Level", c("1", "2", "3", "4", "5"))),
                                column(3, checkboxGroupInput("race", "RACE", c("White", "Black", "Asian or Pacific Islander", "Not White"), selected= "White")),
                                column(3, dateRangeInput("date", "DATE", start= "2019-01-01", end= "2020-04-27", format= "yyyy-mm-dd"))),
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
  
  dateFiltered <- reactive({ cData %>%
      filter(cData$ArrestDate %in% seq(input$date[1], input$date[2], by = "day"))
  })
  
  
  observe({
    
    
    hour <- input$hour
    offenses <- input$repeet
    gender <- input$gender
    date <- dateFiltered()
    violence <- input$violence
    race <- input$race
    
    newPoints <- dateFiltered() %>%
      filter(dateFiltered()$hour %in% hour & dateFiltered()$NumOffenses %in% offenses & dateFiltered()$Sex %in% gender & dateFiltered()$VIOLENCE %in% violence & dateFiltered()$Race %in% race)
    leafletProxy("mymap") %>% clearMarkers() %>%
      addMarkers(lng = newPoints$longitude, lat= newPoints$latitude, popup = paste("ArrestID", newPoints$ArrestID, "<br>",
                                                                                   "ArrestDate:", newPoints$ArrestDate, "<br>",
                                                                                   "Gender:", newPoints$Sex, "<br>",
                                                                                   "Race:", newPoints$Race, "<br>",
                                                                                   "Crime Type:", newPoints$OFFENSE))
  })
  
}

shinyApp(ui, server)