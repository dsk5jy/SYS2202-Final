
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
