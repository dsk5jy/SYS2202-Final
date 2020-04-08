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
#------------- Import arrest data and add city and state data----------
arrests <- read.csv("~/Desktop/arrests-blue.csv")
arrests$address=arrests$ADDRESS
arrests$city= "Charlottesville"
arrests$state= "VA"
arrests<-arrests %>% unite("FullAddress", city:state, sep=", ")
arrests<-arrests %>% unite("CompleteAddress", address:FullAddress, sep=", ")
#------Assign crime scores----------------
arrests$crimescore=" "

#-------------- get geo coordinates from address----------------
geocoded<-data.frame(arrests)
register_google(key="AIzaSyBtSvnbnZzZdQ3K93rjCkW56mY6iXKC0XA")
has_google_key()
for(i in 1:nrow(arrests)){
  result<-geocode(arrests$CompleteAddress[i], output="latlona", source="google")
  arrests$longitude[i]<-as.numeric(result[1])
  arrests$latitude[i]<-as.numeric(result[2])
}
#--------------Omit the NA coordinate values-------------
arrests<-na.omit(arrests)
#---- plot the crime data on the map
cville_map <- get_map(getbb("Charlottesville"), maptype="toner-background")
crimemap<-ggmap(cville_map)+geom_point(data=arrests, aes(x=arrests$longitude, y=arrests$latitude),color="blue")
crimemap
