# Suoraa viivaa halki Suomen kuntien
#
# Tällä sovelluksella voi ideoida reittejä @geowizard keksimään maastokävelyyn.
# Käyttää OpenStreetMap ja Suomen kuntarajoja
# (kiitos https://github.com/geoharo/Geokml)
# Author: Tatu Lindroos

library(shiny)
library(leaflet)
library(sf)

kuntarajat <- sf::st_read('Kuntarajat.kml')

gen <- function(coords, line_start) {
  # Takes one line from kunta edge: start and end are coordinates
  # next to each other (coords is polygon). From this line,
  # takes 1-100 % (randomly) and returns that coordinate

  line_end <- line_start %% nrow(coords) + 1
  
  perc <- (sample(100, 1)) / 100
  
  lng1 <- coords[line_start, 1]
  lng2 <- coords[line_end, 1]
  
  lat1 <- coords[line_start, 2]
  lat2 <- coords[line_end, 2]
  
  lng3 <- lng1 + (lng2 - lng1) * perc
  lat3 <- lat1 + (lat2 - lat1) * perc
  
  return(list(lng = lng3, lat = lat3))
}

ui <- fluidPage(
  titlePanel("Suoraa viivaa halki Suomen kuntien"),
  leafletOutput("mymap"),
  p(),
  
  splitLayout(
    actionButton("recalc", "Uusi reitti"),
    tags$a(
      href='https://github.com/geoharo/Geokml',
      'Kuntarajat.kml',
      target='_blank'
    ),
    tags$a(
      href='https://github.com/taturl/halki-kuntien',
      'github',
      target='_blank'
    ),
    p(style="text-align: right; padding-right: 8px", 'Tatu Lindroos')
  )
)

server <- function(input, output, session) {
  
  sample_kunta_number <- eventReactive(input$recalc, {
    sample(length(kuntarajat$geometry), 1)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    n <- sample_kunta_number()
    kunta_name = kuntarajat$Name[n]
    kunta <- kuntarajat$geometry[n]
    kunta <- as_Spatial(st_zm(kunta))
    
    coords <- kunta[1]@polygons[[1]]@Polygons[[1]]@coords
    
    distinct_lines = sample(nrow(coords), 2)
    
    point1 = gen(coords, line_start = distinct_lines[1])
    point2 = gen(coords, line_start = distinct_lines[2])
    
    title = paste(
      'Kunta:', kunta_name, '<br>',
      'Reitin alku:', point1$lat, point1$lng, '<br>',
      'Reitin loppu:', point2$lat, point2$lng)
    
    leaflet(kunta) %>%
      addTiles() %>%
      addPolygons() %>%
      addPolylines(
        c(point1$lng, point2$lng),
        c(point1$lat, point2$lat),
        color='red') %>%
      addControl(title, position = "bottomleft")
  })
}

shinyApp(ui, server)
