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
#------------- Import arrest data----------
arrests <- read_excel("Downloads/ArrestData_Cville_2019-pres_CLEAN.xlsx")
arrests<-separate(arrests, TIME, into=c("bullshitdate", "time"), sep=" " )
arrests$bullshitdate<-NULL
arrests$HOUR<-NULL
arrests$MINUTE<-NULL
arrests$SECOND<-NULL
arrests$FirstName<-NULL
arrests$LastName<-NULL
arrests$MiddleName<-NULL
arrests$NameSuffix<-NULL
arrests<-separate(arrests, time, into=c("hour", "minute", "second"), sep=":")
arrests
arrests <- read.csv("~/Desktop/arrests-blue.csv")
#-------------- get coordinates
geocoded<-data.frame(arrests)
for(i in 1:nrow(arrests)){
  result<-geocode(arrests$ADDRESS[i], output="latlona", source="google")
  arrests$longitude[i]<-as.numeric(result[1])
  arrests$latitude[i]<-as.numeric(result[2])
  arrests$ADDRESS[i]<-as.character(result[3])
}

