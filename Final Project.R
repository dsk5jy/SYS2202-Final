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
library(leaflet.extras)
#-----------------Get the US county map data and then extract the Virginia counties-----------
counties<-map_data("state")
va_county<-counties%>%filter(region=="virginia")
#----------Plot the Virginia counties Map-------------------
vamap<- ggplot(data=va_county, mapping=aes(x=long, y=lat, group=group))+
  coord_quickmap()+geom_polygon(fill="white", color="black", size=.2)
uscountymap<-ggplot(data=counties, mapping=aes(x=long, y=lat, group=group))+
  coord_quickmap()+geom_polygon(fill="white", color="black", size=.2)
vamap
uscountymap
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
#---- plot the crime data on the map
usmap<-get_map(getbb("United States"), maptype="toner-background")
cville_map <- get_map(location="united states ", maptype="toner-background")
mapjoint<- ggmap(cville_map)+geom_point(data=arrests2, aes(x=arrests2$longitude, y=arrests2$latitude),color="blue")
mapjoint
crimemap<-uscountymap+
  geom_point(data=arrests2, aes(x=arrests2$longitude, y=arrests2$latitude),color="blue", group=group)
crimemap
#--- write to excel to fuck w some stuff
write.csv(arrests2, "Desktop/ArrestData_Cville_2019_WithCoordinates.csv")
# Read it back in--
arrests2<- read.csv("~/Desktop/ArrestData_Cville_2019_WithCoordinates.csv")

### Everything above written by Darren
#----------- Fuck w Shiny and Leaflet --------------------------
arrests2$violencelev <- ifelse(arrests2$VIOLENCE <=2, "mild",
                               ifelse(arrests2$VIOLENCE >2 | arrests2$VIOLENCE <5,"medium",
                                      ifelse(arrests2$VIOLENCE >= 5, "Bad")))

#---- clear outlying values- use the map to look where the values are-----

arrests2<-arrests2[-c(1633, 1634, 1635, 1636, 3295, 3296, 3297, 1445, 1922, 1372, 586, 607, 1213, 1214),]
arrests3<-arrests2[-607]
bbox_USA = c(left = -124.7844079,
             bottom = 24.7433195,
             right = -66.9513812,
             top = 49.3457868)

USA = get_map(bbox_USA, source='stamen')
jaunt<-ggmap(USA)+geom_point(data=arrests3, aes(x=longitude, y=latitude),color="blue")
jaunt
for (i in length(arrests2)){
  arrests2$longitude[arrests2$longitude %in% "-79.94764"] <-NA
}
arrests2<-na.omit(arrests2)
min(arrests2$longitude)

#--------- shiny app creation------------
ui <- fluidPage(
  titlePanel("Charlottesville Crime Map"),
  verticalLayout(sidebarPanel("OPTIONS", 
                              fluidRow(
                                column(3, sliderInput("slider", "Time of Day", min = 0, max = 23, value = arrests2$hour)),
                                column(3, checkboxGroupInput("gender", "Gender", c("Male", "Female"), select = "Male")),
                                column(3, sliderInput("repeet", "Repeat Offenses", min= 1, max = 20, value = arrests2$NumOffenses)),
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
      addMarkers(lng = arrests2$longitude, lat= arrests2$latitude)
  })
  
  observe({
    
    hour <- input$hour
    offenses <- input$repeet
    gender <- input$gender
    date <- input$date
    violence <- input$violence
    race <- input$race
    
    newPoints <- arrests2 %>%
      filter(arrests2$ArrestDate %in% date & arrests2$hour %in% hour & arrests2$NumOffenses %in% offenses & arrests2$Sex %in% gender & arrests2$VIOLENCE %in% violence & arrests2$Race %in% race)
    
    leafletProxy("mymap") %>% clearMarkers() %>%
      addMarkers(lng = newPoints$longitude, lat= newPoints$latitude, popup = paste("ArrestID", arrests2$ArrestID, "<br>",
                                                                                   "ArrestDate:", arrests2$ArrestDate, "<br>",
                                                                                   "Gender:", arrests2$Sex, "<br>",
                                                                                   "Race:", arrests2$Race))
  })
  
}

shinyApp(ui, server)
