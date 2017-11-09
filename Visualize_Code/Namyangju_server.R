library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- vc

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 127.4962945, lat = 37.8372685, zoom = 11)
  })
  
  # Choose just one type
  drawvalue <- reactive({if (input$type == ''){return(vc)}else{
    t <- filter(vc, assets_name == input$type)
    return(t)
  }})
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    colorBy <- input$color
    sizeBy <- input$size
    draw <- drawvalue()
    
    colorData <- draw[[colorBy]]
    # if (colorBy == "NUMBER.OF.PERSONS.INJURED"|colorBy == "NUMBER.OF.PERSONS.KILLED") {
    #   pal <- colorBin(heat.colors(7), colorData, 7)} else{
    #     pal <- colorFactor("Set1", colorData)
    #   }    
    
    radius <- draw[[sizeBy]] / 9 * 250 + 30
    
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~LONGITUDE, ~LATITUDE, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }
  })
  
  # Show a popup at the given location
  showvcPopup <- function(eventid, lat, lng) {
    draw <- drawvalue()
    selectedvc <- filter(draw, LATITUDE == lat, LONGITUDE == lng)
    entry <- function(row){
      result <- as.character(tagList(
        tags$h6(row[2], row[3]), 
        tags$strong(HTML(sprintf("%s & %s", row[9], row[10]))), tags$br(),
        sprintf("Vehicles: %s & %s", row[26], row[27]), tags$br(),
        sprintf("Factors: %s & %s", row[20], row[21]), tags$br(),
        sprintf("%s Injuries & %s Deaths", row[12], row[13]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = eventid)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })
})