library(dplyr)

# some required functions
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

#Import public_asset data
files = dir('data/')
data_list = list()
Public_Assets = data.frame()
for(i in 1:length(files)){
  data_list[[i]] = read.csv(paste0('data/',files[i]))
  tmp = data_list[[i]]
  tmp = data.frame(tmp$city, tmp$latitude, tmp$longitude, tmp$공부지목명, tmp$재산면적, tmp$대장가액.원., tmp$avg_price)
  colnames(tmp) <- c('도시','lat', 'lon', '재산명칭', 'size', '대장가액', '평균가격')
  tmp = tmp[!duplicated(tmp[,c('lat','lon')]),]
  tmp$평균가격 = round(tmp$평균가격)
  tmp$재산명칭 <- as.character(tmp$재산명칭)
  tmp$도시 <- as.character(tmp$도시)
  tmp$radius = getRad(tmp$size)
  tmp$면적당평균가 = get_avg_price(tmp$대장가액,tmp$size)
  Public_Assets = rbind(Public_Assets,tmp)
}
 vector <- c("park","factory", "orchard", "googeo", "dap", "dae", "road","pasture","cemetery", "unknown", "water","fishery", "oil","forest","mixed","jeon", "levee","religion","parking", "garage", "rail","sports", "river","school")
 vector0 <-c('공원용지','공장용지','과수원','구거','답','대','도로','목장용지','묘지','미등록','수도용지','양식장','유지','임야','잡종지','전','제방','종교용지','주차장','창고용지','철도용지','체육용지','하천','학교용지')

a <- Public_Assets$재산명칭
for(i in 1:length(a)){
  a[i]<- vector0[vector==a[i]] 
}
Public_Assets$재산명칭 <- a
 
 vector1 <- c("Gapyeong","Gimpo","Goyang","Gwangju","Namyangju")
 vecotr2 <- c("가평","김포","고양","광주","남양주")
b <- Public_Assets$도시
for(i in 1:length(b)){
  b[i]<- vecotr2[vector1==b[i]]
}
Public_Assets$도시 <- b