#키 가져오기: AIzaSyCa1ZzzzLT7195yq9FpWiy0Tu6AflmiZHY -> 각자가 구글맵 들어가서 자신의 api key 받아야 함.


#주변 검색 요청: https://maps.googleapis.com/maps/api/place/nearbysearch/output?parameters ->기본 포맷.

library(ggmap)
library(jsonlite)
#category: 구글맵 주변탐색에서 지원하는 장소 유형.
category = c("accounting","airport","amusement_park","aquarium","art_gallery","atm","bakery","bank","bar","beauty_salon","bicycle_store",
             "book_store","bowling_alley","bus_station","cafe","campground","car_dealer","car_rental","car_repair","car_wash","casino",
             "cemetery","church","city_hall","clothing_store","convenience_store","courthouse","dentist","department_store","doctor",
             "electrician","electronics_store","embassy","fire_station","florist","funeral_home","furniture_store","gas_station",
             "gym","hair_care","hardware_store","hindu_temple","home_goods_store","hospital","insurance_agency","jewelry_store",
             "laundry","lawyer","library","liquor_store","local_government_office","locksmith","lodging","meal_delivery","meal_takeaway",
             "mosque","movie_rental","movie_theater","moving_company","museum","night_club","painter","park","parking","pet_store",
             "pharmacy","physiotherapist","plumber","police","post_office","real_estate_agency","restaurant","roofing_contractor",
             "rv_park","school","shoe_store","shopping_mall","spa","stadium","storage","store","subway_station","synagogue","taxi_stand",
             "train_station","transit_station","travel_agency","university","veterinary_care","zoo")
api_key = "AIzaSyCa1ZzzzLT7195yq9FpWiy0Tu6AflmiZHY"   #키 가져오기를 통해 불러온 api key.
rad = 500   #주변 검색시 반경 설정.
locate = geocode(enc2utf8('수원역'), source="google") #주변 검색을 실시하고자 하는 위치 정보(위,경도). 공유재산 위치정보. 

radarsearch <- function(lat, lon, rad, type){  #우선 특정 위치정보&반경거리 입력했을 때, 모든 카테고리 기준으로 주변 탐색하여 결과값 저장해주는 함수.
  #여러 위치에 대해, 즉 공유재산현황데이터에서 모든 위치에 대해 주변탐색 실시하고자 하면 위치정보 백터로 받아놓고
  #그만큼 for문 돌리면 됨.
  result = list()
  url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json","?location=",lat,",",lon,"&radius=",rad,"&types=",type,"&key=",api_key)
  raw.data = readLines(url, warn = "F", encoding="UTF-8")
  dat = fromJSON(raw.data)
  result = dat
  return(result) 
}

result = radarsearch(locate$lat, locate$lon, rad, "restaurant")
str(result)
result$results$geometry

###카테고리 군집별 비율 구하기
airport = c("airport")
culture = c("amusement_park","aquarium","art_gallery","bowling_alley","campground","casino","movie_rental","movie_theater","museum",             
            "night_club","park","rv_park","stadium","zoo")
food = c("bakery","bar","cafe","meal_delivery", "meal_takeaway","restaurant")
community = c("beauty_salon","bicycle_store","book_store","clothing_store","convenience_store","electrician","electronics_store",
              "car_dealer","car_rental","car_repair","car_wash","dentist","department_store","doctor","florist","furniture_store",             
              "gas_station","gym","hair_care","hardware_store","home_goods_store","jewelry_store","laundry","liquor_store",
              "locksmith","lodging","painter","pet_store","pharmacy","physiotherapist","plumber","police","post_office","shoe_store",
              "shopping_mall","spa","storage","store","veterinary_care","bank","atm")
death = c("cemetery","funeral_home")
religion = c("church","hindu_temple","mosque","synagogue")
government = c("city_hall","courthouse","embassy","fire_station","local_government_office")
work = c("real_estate_agency","roofing_contractor","moving_company","travel_agency","accounting","insurance_agency","lawyer")
hospital = c("hospital")
edu = c("library","school","university")
parking = c("parking")
station = c("taxi_stand","train_station","transit_station","subway_station","bus_station")

category_cnt <- function(lat, lon, rad, type){  
  result = list()
  cnt = c()
  for(i in 1:length(type)){
  url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json","?location=",lat,",",lon,"&radius=",rad,"&types=",type[i],"&key=",api_key)
  raw.data = readLines(url, warn = "F", encoding="UTF-8")
  dat = fromJSON(raw.data)
  result = dat
  cnt = cbind(cnt, nrow(result$results))
  sum = sum(cnt)
  }
  return(sum) 
}
category_cnt(locate$lat, locate$lon, rad, food)
str(airport)
###12개 중분류 카테고리 전체 넣고 돌렸을 때, 12개 중 각 카테고리의 비율
types = list(airport=airport,culture=culture,food=food,community=community,death=death,religion=religion,government=government,
             work=work,hospital=hospital,edu=edu,parking=parking,station=station)

category_ratio <- function(lat, lon, rad, types){  
  total = c()
  ratio = c()
  for(i in 1:length(types)){
    cnt = category_cnt(lat, lon, rad,types[[i]])
    total = c(total,cnt)
  }
  total_cnt = sum(total)
  for(i in 1:length(types)){
    ratio = c(ratio,total[i]/total_cnt)
  }
  return(ratio) 
}

ratio = category_ratio(locate$lat, locate$lon, rad, types)

