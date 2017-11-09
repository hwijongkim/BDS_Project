#leaflet 사용자 패키지
library(htmltools)
library(dplyr)
library(leaflet)

#1 가평군의 공유재산 수량 및 분포
m1 <- read.csv("Project/시각화 코드/Gapyeong.csv")
m1$avg_price = (m1$대장가액.원./m1$재산면적)
m1$대장가액.원. <- ifelse(is.na(m1$대장가액.원.), 0, m1$대장가액.원.)
m1 <- data.frame(m1$latitude, m1$longitude, m1$공부지목명, m1$재산면적, m1$대장가액.원., m1$avg_price)
colnames(m1) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
m1$size <- ifelse(is.na(m1$size), 1, m1$size)
m1$avg_price <- ifelse(is.na(m1$avg_price), 0, m1$avg_price)
m1$avg_price <- ifelse(m1$avg_price == "Inf", 1, m1$avg_price)
m1$avg_price = round(m1$avg_price)
m1$assets_name <- as.character(m1$assets_name)
m1$popup <- paste("assets_name: ", m1$assets_name, " size: ", m1$size, " tot_price: ", m1$tot_price, " avg_price: ", m1$avg_price)

op <- leaflet(m1)
op <- op %>% addTiles() %>% setView(lng = 127.4962945, lat = 37.8372685, zoom = 11)
op %>% addTiles() %>% addMarkers(~lon, ~lat, clusterOptions = markerClusterOptions(), popup = ~htmlEscape(popup))
