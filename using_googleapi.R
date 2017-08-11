###############################################################################
## google api를 이용하는 함수들 모음입니다.
## 사용하기전 checklist
##  1. api key가 제대로 설정되었는지 확인
##  2. working directory 확인
##
## ---------------------------------------------------------------------------
## search_details => returns detail information of places
## radarsearch => 주변지역 200개 검색(category 별로)
## category_cnt => 주어진 category에 대한 결과값 수
## category_ratio => category별 빈도수 계산
###############################################################################

library(httr)
library(jsonlite)
## Categories
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

key <- readLines('google_apis.txt')

search_details <- function(key,placeid,return_type=c("json","xml")){
    ## key = api key
    ## placeid = one place id or vector of place ids
    n <- length(placeid)
    url <- "https://maps.googleapis.com/maps/api/place/details/"
    result <- list()
    for(i in 1:n){
        request_url <- paste0(url,return_type,"?placeid=",placeid[i],
                              "&key=",key)
        return_json <- GET(request_url)
        details <- fromJSON(toJSON(return_json))
        if (details$status == "OK"){
            result[[i]] <- details
        }else{
            result[[i]] <- details$status
        }
    }
    ## returns list of detail information of place(s)
    return(result)
}

radarsearch <- function(api_key,lat, lon, rad, type){  
    #우선 특정 위치정보&반경거리 입력했을 때, 모든 카테고리 기준으로 주변 탐색하여 결과값 저장해주는 함수.
    #여러 위치에 대해, 즉 공유재산현황데이터에서 모든 위치에 대해 주변탐색 실시하고자 하면 위치정보 백터로 받아놓고
    #그만큼 for문 돌리면 됨.
    result = list()
    url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json",
                 "?location=",lat,",",lon,"&radius=",rad=1000,"&types=",type,
                 "&key=",api_key)
    raw.data = readLines(url, warn = "F", encoding="UTF-8")
    dat = fromJSON(raw.data)
    result = dat
    return(result) 
}

category_cnt <- function(key,lat, lon, rad=1000, type){  
    result = list()
    cnt = c()
    for(i in 1:length(type)){
        url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json",
                     "?location=",lat,",",lon,"&radius=",rad,"&types=",
                     type[i],"&key=",key)
        raw.data = readLines(url, warn = "F", encoding="UTF-8")
        dat = fromJSON(raw.data)
        result = dat
        cnt = cbind(cnt, nrow(result$results))
        sum = sum(cnt)
    }
    return(sum) 
}

category_ratio <- function(key,lat, lon, rad=1000, types){  
    total = c()
    ratio = c()
    for(i in 1:length(types)){
        cnt = category_cnt(key,lat, lon, rad,types[[i]])
        total = c(total,cnt)
    }
    total_cnt = sum(total)
    for(i in 1:length(types)){
        ratio = c(ratio,total[i]/total_cnt)
    }
    return(ratio) 
}