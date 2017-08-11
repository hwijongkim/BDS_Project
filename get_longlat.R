library(httr)
library(jsonlite)
files <- dir('gg_public_assets')
data_list <- list()

clientID <- read.csv('naver_api.txt',header=FALSE,stringsAsFactor=FALSE)[2,1]
clientSecret <-read.csv('naver_api.txt',header=FALSE,
                        stringsAsFactor=FALSE)[2,2]

for(i in 1:32){
  data_list[[i]] <- read.csv(paste('gg_public_assets/',files[i],sep=""))
  df <- data_list[[i]]
  df$address <- paste(df$시군구명,df$읍면동명,df$리명,df$본번,"-",df$부번)
  data_list[[i]] <- df
}

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

# for(i in 1:length(data_list)){
#     lonlat <- get_geocode(data_list[[i]]$address)
#     data_list[[i]]$longitude <- lonlat[,1]
#     data_list[[i]]$latitude <- lonlat[,2]
# }
# 
# ## write csv file
# for(i in 1:length(data_list)){
#   data_list[[i]]$latitude = as.numeric(data_list[[i]]$latitude)
#   data_list[[i]]$longitude = as.numeric(data_list[[i]]$longitude)
#   write.csv(paste(data_list[[i]],'public_assets_lonlat/',files[i],sep=""))
# }

