library(httr)
library(jsonlite)
files <- dir('프로젝트 데이터/공유재산현황_시군구별')
data_list <- list()

clientID <- read.csv('프로젝트 데이터/naver_api.txt',header=FALSE,stringsAsFactor=FALSE)[2,1]
clientSecret <-read.csv('프로젝트 데이터/naver_api.txt',header=FALSE,
                        stringsAsFactor=FALSE)[2,2]

  data_list[[21]] <- read.csv(paste('프로젝트 데이터/공유재산현황_시군구별/',files[21],sep=""))
  df <- data_list[[21]]
  df$address <- paste(df$시군구명,df$읍면동명,df$리명,df$본번,"-",df$부번)
  data_list[[21]] <- df
  View(data_list[[21]])

get_geocode <- function(adr){
  ## adr is vector of addresses
  n <- length(adr)
  api <- 'https://openapi.naver.com/v1/map/geocode'
  lonlat <- data.frame()
  for(i in 1:n){
    adr_i <- adr[i]
    adr_i <- gsub(" - 0","",adr_i)
    adr_i <- URLencode(adr_i)
    request_url <- paste(api,"?query=",adr_i,sep="")
    geo_json <- GET(request_url,
                    add_headers('X-Naver-Client-Id'=clientID,
                                'X-Naver-Client-Secret'=clientSecret))
    if(geo_json$status_code == 200){
      geo <- fromJSON(toJSON(content(geo_json)))
      lonlat <- rbind(lonlat,geo$result$items$point)
    } else {
      ## If error occurs
      lonlat <- rbind(lonlat,c(x=999,y=999))
    }
  }
  return(lonlat)
}

lonlat <- get_geocode(data_list[[21]]$address)
data_list[[21]]$longitude <- lonlat[,1]
data_list[[21]]$latitude <- lonlat[,2]


## write csv file
data_list[[21]]$latitude = as.numeric(data_list[[21]]$latitude)
data_list[[21]]$longitude = as.numeric(data_list[[21]]$longitude)
write.csv(data_list[[21]],files[21])

