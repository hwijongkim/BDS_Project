###############################################################################
## clustering
##

library(cluster)
?hclust
?dist
sub <- ana_data[,-c(1,2,3,4)]
sub2 <- ana_data[,c(1)]
d_matrix <- dist(sub)
str(d_matrix)
h <- hclust(d_matrix)
plot(h)
clustercut <- cutree(h,4)
h_4 <- as.data.frame(cbind(sub2,clustercut))
h_4$clustercut <- as.factor(h_4$clustercut)
str(h_4)

result <- cbind(h_4,ana_data[,c(3,4)])

write.csv(result,'clusterd.csv',fileEncoding = "UTF-8")
