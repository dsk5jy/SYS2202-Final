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
write.csv(arrests2, "Desktop/ArrestData_Cville_2019_WithCoordinates")


### Everything above written by Darren