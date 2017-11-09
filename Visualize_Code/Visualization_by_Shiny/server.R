library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- Public_Assets

# 불러온 데이터를 샤이니 앱과 연동하여 돌아가게 하는 서버입니다.
shinyServer(function(input, output, session) {
  
  
  # leaflet 지도를 출력합니다.
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 127.1949, lat = 37.6454, zoom = 10)
  })
  
  # UI 상에서 보고자 하는 시를 클릭할 때마다 서버가 반응하여 해당하는 시의 데이터를 불러옵니다. 
  drawvalue <- reactive({
    if (input$city == 'All') {
      t <- Public_Assets
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
    }
    else if (input$city == 'Gapyeong') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
    }
    else if (input$city == 'Gimpo') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
      }
    else if (input$city == 'Goyang') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
      }
    else if (input$city == 'Gwangju') {
      t <- filter(Public_Assets, city == input$city)
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
      }
    else {
      t <- filter(Public_Assets, city == input$city)
      
      if (input$type == ''){return(t)}else{
        t <- filter(t, assets_name == input$type)
        return(t)}
      }
  })
  
  # User가 선택한 데이터 정보를 지도에 원으로 표시합니다. 이 때, UI 상의 legend에서 
  # 특정 재산 형태를 클릭하면 해당하는 공유 재산만을 지도에 뿌려줍니다.
  # 또한 cluster를 클릭할 경우, 주변 공유 재산끼리 묶어 다각형 형태로 군집화하여 지도에 표시해 줍니다.
  observe({
    
    sizeBy <- input$size
    draw <- drawvalue()
    colorBy <- "assets_name"
    
    colorData <- draw[[colorBy]]
    set = c(brewer.pal(12,"Paired"),brewer.pal(6,"Set2"),brewer.pal(7, "Set3"))
  
    if(length(unique(colorData))==1){
      pal <- colorFactor(set[sample((1:25),1)], colorData)
    }
    else{
      pal <- colorFactor(set, colorData)
    }
       
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~lon, ~lat, radius=draw[[sizeBy]], group = "Circle",
                    stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~lon, ~lat, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~lon, ~lat, radius=draw[[sizeBy]], group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }
  })
  
  # 지도 상에서 특정 공유 재산 원을 클릭했을 때, 해당 공유 재산의 위/경도 및 기본 정보가
  # 팝업 형식으로 지도 위에 출력됩니다.
  showvcPopup <- function(eventid, latitude, longitude) {
    draw <- drawvalue()
    selectedvc <- filter(draw, lat == latitude, lon == longitude)
    entry <- function(row){
      result <- as.character(tagList(
        tags$strong(sprintf("assets_name: %s", row[4])), tags$br(),
        sprintf("lat | lon: %s | %s", row[2],row[3]), tags$br(),
        sprintf("size: %s", row[5]), tags$br(),
        sprintf("tot_price: %s", row[6]), tags$br(),
        sprintf("avg_price: %s", row[7]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(longitude, latitude, content, layerId = eventid)
  }

  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })
  
  ## See Your Neighbourhood ###########################################

  observe({
    assets <- if (is.null(input$cities)) character(0) else {
      filter(Public_Assets, city %in% input$cities) %>%
        `$`('assets_name') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$assets[input$assets %in% assets])
    updateSelectInput(session, "assets", choices = assets,
                      selected = stillSelected)
  })

  # When actions is clicked, call popup function for the corresponding latitude and longitude
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.02
      event <- input$map_shape_click
      lat <- input$goto$lat
      lng <- input$goto$lng
      showvcPopup(event$id, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$P.A.table <- DT::renderDataTable({
    df <- Public_Assets %>%
      filter(
        is.null(input$cities) | city %in% input$cities,
        is.null(input$assets) | assets_name %in% input$assets
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '" data-city="', city, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
})
