# Group Project

install.packages('leaflet')

library(tidyverse)
library(leaflet)
data("museum.csv")

Offense_Data_Virginia_2018

museum <- Offense_Data_Virginia_2018
museum %>% leaflet() %>% addProviderTiles(providers$Stamen.TonerLite)

