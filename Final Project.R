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

#-----------------Get the US county map data and then extract the Virginia counties-----------
counties<-map_data("county")
va_county<-counties%>%filter(region=="virginia")
#----------Plot the Virginia counties Map-------------------
vamap<- ggplot(data=va_county, mapping=aes(x=long, y=lat, group=group))+
  coord_quickmap()+geom_polygon(fill="white", color="black", size=.2)
vamap
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
cville_map <- get_map(getbb("Charlottesville"), maptype="toner-background")
crimemap<-ggmap(cville_map)+geom_point(data=arrests2, aes(x=arrests2$longitude, y=arrests2$latitude),color="blue")
crimemap
#--- write to excel to fuck w some stuff
write.csv(arrests2, "Desktop/ArrestData_Cville_2019_WithCoordinates.csv")
# Read it back in--
arrests2<- read.csv("~/Desktop/ArrestData_Cville_2019_WithCoordinates.csv")

### Everything above written by Darren

arrests2$violencelev <- ifelse(arrests2$VIOLENCE <=2, "mild",
                               ifelse(arrests2$VIOLENCE >2 | arrests2$VIOLENCE <5,"medium",
                                      ifelse(arrests2$VIOLENCE >= 5, "Bad")))



ui <- fluidPage(
  mainPanel(
    leafletOutput(outputId = "mymap"),
    absolutePanel(top = 60, left = 20,
                  checkboxInput("markers","VIOLENCE",FALSE),
                  checkboxInput("heat","violencelev",FALSE)
                  
                )
  )
)

server <- function(input,output,session){
  pal<- colorNumeric(
    palette = c('green','yellow','red'),
                domain = arrests2$VIOLENCE)
  pal2 <- colorFactor(
    palette = c('green','yellow','red'),
    domain = arrests2$violencelev
  )
  
  output$mymap <- renderLeaflet({
    leaflet(arrests2) %>%
      setView(lng = -78.49,lat = 38.03, zoom = 2) %>%
      addTiles() %>%
      addCircles(data = arrests2, lat = ~ latitude, lng = ~ longitude,
                 weight = 1, radius = ~sqrt(VIOLENCE)*40, popup = ~as.character(VIOLENCE),
                 label = ~as.character(paste0("Violence: ",sep = " ", VIOLENCE)), color = ~ pal(VIOLENCE),fillOpacity = 0.5)
  })
  
  observe({
    proxy<- leafletProxy("mymap",data = arrests2)
    proxy %>% clearMarkers()
    if(input$markers){
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~ pal2(violencelev), fillOpacity = 0.2,
                    label = ~as.character(paste0("Violence: ", sep = " ", VIOLENCE)))%>%
                      addLegend("bottomright", pal = pal2, values = arrests2$violencelev,
                                title = "Violence",
                                opacity = 1)}
    else{
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy<- leafletProxy("mymap", data = arrests2)
    proxy %>% clearMarkers()
    if(input$heat){
      proxy %>% addHeatmap(lng = ~longitude, lat = ~latitude, 
        intensity = ~VIOLENCE, blur = 10, max = 0.05, radius = 15)
    }
    else{
      proxy %>% clearHeatmap()
    }
  })
  
}

shinyApp(ui,server)

#Code above by hunter diminick
