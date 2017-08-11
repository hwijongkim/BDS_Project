## Make datasets for clustering

## Read dataset and make subset with 2 columns
# data <- read.csv('Data/ggpa2/경기도 하남시.csv',stringsAsFactor=FALSE)

## dataframe에서 실지목명과 공부지목명이 도로를 제외한 나머지 행을 리턴합니다.
## 재산번호, address 컬럼만 포함
make_subset <- function(df){
    sub1 <- df[!is.na(data$재산면적),c("재산번호","실지목명",
                                   "공부지목명","address","재산면적","longitude","latitude")]
    sub2 <- sub1[sub1$실지목명!='도로'&sub1$공부지목명!='도로',]
    sub3 <- sub2[,-c(2,3)]
    return(sub3)
}

## get longitude and latitude using naver api
# source('get_longlat.R')
# sub <- make_subset(data)
# lonlat <- get_geocode(sub$address)
# str(lonlat)
# sub2 <- cbind(sub,lonlat)
# head(sub2)
# str(sub2)

## nearbysearch -> compute sum by category
source('using_kakaoapi.R')

sum_by_category <- function(df){
    totals_ <- data.frame()
    start <- Sys.time()
    for(i in 1:nrow(df)){
        x = df$longitude[i]
        y = df$latitude[i]
        counts_ <- c()
        for(j in 1:length(categories)){
            counts_ <- c(counts_,get_total_count(categories[j],x,y,radius=5000))
        }
        totals_ <- rbind(totals_,counts_)
    }
    print(Sys.time()-start)
    colnames(totals_) <- categories
    return(totals_)
}
# totals_ <- sum_by_category(sub2)
# str(totals_)
## cbind with subset
# ana_data <- cbind(sub3,totals_)
