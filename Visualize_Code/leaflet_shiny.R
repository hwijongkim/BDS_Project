##########3. leaflet 사용 지도시각화##########
#데이터 영문으로 바꿔야 인식함
#leaflet 사용자 패키지
library(htmltools)
library(dplyr)
library(leaflet)

#1 가평군
m1 <- read.csv("Gapyeong.csv")
m1 <- m1[sample(nrow(m1),100),]
m1$avg_price = (m1$대장가액.원./m1$재산면적)
m1$대장가액.원. <- ifelse(is.na(m1$대장가액.원.), 0, m1$대장가액.원.)
m1 <- data.frame(m1$latitude, m1$longitude, m1$공부지목명, m1$재산면적, m1$대장가액.원., m1$avg_price)
colnames(m1) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m1$size <- ifelse(is.na(m1$size), 1, m1$size)
m1$avg_price <- ifelse(is.na(m1$avg_price), 0, m1$avg_price)
m1$avg_price <- ifelse(m1$avg_price == "Inf", 1, m1$avg_price)
m1$avg_price <- ifelse(m1$avg_price > 5000000, 1, m1$avg_price)
m1$avg_price = round(m1$avg_price)
#m1$color <- ifelse(m1$assets_name == "ddae", "navy", "")
#m1$color <- ifelse(m1$assets_name == "ddap", "gold", "")
#m1$color <- ifelse(m1$assets_name == "forest", "green", "")
#m1$color <- ifelse(m1$assets_name == "jeon", "blue", "")
#m1$color <- ifelse(m1$assets_name == "levee", "red", "")
#m1$color <- ifelse(m1$assets_name == "random", "black", "")
#m1$color <- ifelse(m1$assets_name == "river", "white", "")
#m1$color <- ifelse(m1$assets_name == "road", "pink", "")
#m1$color <- ifelse(m1$assets_name == "unknown", "brown", "")
m1$assets_name <- as.character(m1$assets_name)
m1$popup <- paste("assets_name: ", m1$assets_name, " size: ", m1$size, " tot_price: ", m1$tot_price, " avg_price: ", m1$avg_price)

#공유재산별 분포 시각화
#m1 <- m1[which(m1$assets_name==c("ddap","jeon","unknown")), 
#         c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')]

#1 가평군의 공유재산 수량 및 분포
op <- leaflet(m1)
op <- op %>% addTiles() %>% setView(lng = 127.4962945, lat = 37.8372685, zoom = 11)
op %>% addTiles() %>% addMarkers(~lon, ~lat, clusterOptions = markerClusterOptions(), popup = ~htmlEscape(popup))

#2 가평군의 공유재산별 컬러 시각화
col <- c("ddae"="navy", "ddap"="gold", "forest"="green")
pal <- colorFactor(c("navy", "gold", "green", "blue", "red", "black", 
                     "white", "pink", "brown"), domain = c("ddae", "ddap", "forest", "jeon", "levee", "random", "river", "road", "unknown"))
op %>% addTiles() %>%addCircleMarkers(radius = 5, color = ~pal(m1$assets_name), 
                                      stroke = FALSE, fillOpacity = 0.5, popup = ~htmlEscape(popup) )
#3 shiny 연결
library(shiny)
#library(leaflet)
library(RColorBrewer)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "avg_price", min(m1$avg_price), max(m1$avg_price),
                            value = range(m1$avg_price), step = 10000),
                selectInput("name", "assets_name", col),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    m1[m1$avg_price >= input$range[1] & m1$avg_price <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$name, m1$avg_price)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    if(input$name == "ddae"){
      m1 <- subset(m1, m1$assets_name =="ddae")
    }
    leaflet(m1) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~m1$avg_price/1000, weight = 5, color = "#777777",
                 fillColor = ~pal(m1$avg_price), fillOpacity = 0.7, popup = ~paste(popup)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = m1)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~m1$avg_price
      )
    }
  })
}
tempdir()
shinyApp(ui, server)


