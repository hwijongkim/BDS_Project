library(dplyr)

# 공유 재산들이 각각의 면적에 맞게 지도 상에 적당한 크기로 보여질 수 있도록
# 원의 크기를 조정해주는 함수입니다.
getRad = function(size_vector){
  size_to_rad = c()
  for(i in 1:length(size_vector)){
    x = size_vector[i]
    if(x >= 10000000) {x = 500}
    else if(x >= 5000000) {x = 380}
    else if(x >= 2000000) {x = 300}
    else if(x >= 800000) {x = 250}
    else if(x >= 500000) {x = 220}
    else if(x >= 200000) {x = 190}
    else if(x >= 50000) {x = 160}
    else if(x >= 10000) {x = 120}
    else if(x >= 5000) {x = 100}
    else if(x >= 1000) {x = 80}
    else if(x >= 500) {x = 65}
    else if(x >= 100) {x = 50}
    else x
    size_to_rad = c(size_to_rad, x)
  }
  return(size_to_rad)
}

# 공유 재산들이 각각의 단위 가격에 맞게 지도 상에 적당한 크기로 보여질 수 있도록 
# 원의 크기를 조정해주는 함수입니다.
get_avg_price = function(price,size){
  avg_p = c()
  for(i in 1:length(price)){
    x = price[i]/size[i]
    if(x == Inf) {x=0}
    else if(x >= 10000000) {x = 500}
    else if(x >= 5000000) {x = 380}
    else if(x >= 2000000) {x = 300}
    else if(x >= 800000) {x = 250}
    else if(x >= 500000) {x = 220}
    else if(x >= 200000) {x = 190}
    else if(x >= 50000) {x = 160}
    else if(x >= 10000) {x = 120}
    else if(x >= 5000) {x = 100}
    else if(x >= 1000) {x = 80}
    else if(x >= 500) {x = 65}
    else if(x >= 100) {x = 50}
    else x
    avg_p = c(avg_p, x)
  }
  return(avg_p)
}

# 처리된 데이터를 불러와 필요한 피쳐에 해당하는 데이터를 추려 하나의 데이터 프레임으로 만들어 줍니다.
files = dir('data/')
data_list = list()
Public_Assets = data.frame()
for(i in 1:length(files)){
  data_list[[i]] = read.csv(paste0('data/',files[i]))
  tmp = data_list[[i]]
  tmp = data.frame(tmp$city, tmp$latitude, tmp$longitude, tmp$공부지목명, tmp$재산면적, tmp$대장가액.원., tmp$avg_price)
  colnames(tmp) <- c('city','lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
  tmp = tmp[!duplicated(tmp[,c('lat','lon')]),]
  tmp$avg_price = round(tmp$avg_price)
  tmp$assets_name <- as.character(tmp$assets_name)
  tmp$city <- as.character(tmp$city)
  tmp$radius = getRad(tmp$size)
  tmp$avg_price_size = get_avg_price(tmp$tot_price,tmp$size)
  Public_Assets = rbind(Public_Assets,tmp)
}
