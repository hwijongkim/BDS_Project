source('get_longlat.R')
source('using_kakaoapi.R')
source('make_data_for_clustering.R',encoding = "UTF-8")
data <- read.csv('Data/ggpa2/경기도 구리시.csv',stringsAsFactor = FALSE)
sub <- make_subset(data)
lonlat <- get_geocode(sub$address)
sub <- cbind(sub,lonlat)
total <- sum_by_category(sub)

ana_data1 <- cbind(sub,total)
ana_data2 <- cbind(sub,total)

size_factor <- sapply(ana_data2$재산면적,function(x) {
    if(x<300) x=1
    else if(x>=300 && x<500) x=2
    else if(x>=500 && x<1000) x=3
    else x=4
})
ana_data2$재산면적 <- as.factor(size_factor)
str(ana_data2)

## clustering
library(cluster)
d_matrix <- daisy(ana_data2[,-c(1,2)],metric="gower")
h <- hclust(d_matrix)
plot(h)
library(fpc)

clust <- numeric(20)
for(k in 2:20){
    clust[[k]] <- pam(d_matrix,k)$silinfo$avg.width
    k.best <- which.max(clust)
}
k.best
