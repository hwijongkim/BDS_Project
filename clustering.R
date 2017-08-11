source('get_longlat.R', encoding='UTF-8')
source('using_kakaoapi.R')
source('make_data_for_clustering.R',encoding = "UTF-8")
data <- read.csv('include_lonlat/경기도 동두천.csv',stringsAsFactor = FALSE)
sub <- make_subset(data)
View(sub)
#lonlat <- get_geocode(sub$address)   #위,경도 없는 csv 파일인 경우.
#sub <- cbind(sub,lonlat)
total <- sum_by_category(sub)

ana_data1 <- cbind(sub,total)
ana_data2 <- cbind(sub,total)
View(ana_data2)
size_factor <- sapply(ana_data2$재산면적,function(x) {
    if(x<300) x=1
    else if(x>=300 && x<500) x=2
    else if(x>=500 && x<1000) x=3
    else x=4
})
ana_data2$재산면적 <- as.factor(size_factor)
str(ana_data2)

## clustering -> hclust
library(cluster)
d_matrix <- daisy(ana_data2[,-c(1,2)],metric="gower")

h <- hclust(d_matrix)
plot(h)
## clustering -> kmeans
k = kmeans(d_matrix, centers = 4, iter.max = 10000)
k$cluster = as.factor(k$cluster)

## find most fitted num. of clust
library(fpc)
clust <- numeric(20)
for(k in 4:20){
    clust[[k]] <- pam(d_matrix,k)$silinfo$avg.width
    k.best <- which.max(clust)
}
k.best

View(ana_data2)
#0.9, 4
d = dbscan(d_matrix, eps=0.8, MinPts = 8)
str(d)



