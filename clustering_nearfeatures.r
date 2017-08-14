data1 = read.csv('Project/ana_data/경기도 가평군_r1000.csv')
View(data1)

data1 = data1[which(data1$valueBysize1!=Inf),]
View(data1)
z_data = data1
z_data = scale(z_data[,6:25])
View(z_data)
data1[,6:25] = z_data
data2 = data1[,-c(1,2,3,4,6)]
View(data2)
size1 = data2[which(data2$size_factor==1),]
size2 = data2[which(data2$size_factor==2),]
size3 = data2[which(data2$size_factor==3),]
size4 = data2[which(data2$size_factor==4),]
View(size1)
#d_matrix = daisy(data1[,-c(1,2,3,4)],metric='gower')
#z_data = data1[,-c(1,2,3,4,5)]
library(cluster)

d_matrix1 = daisy(size1, metric='gower')
d_matrix2 = daisy(size2, metric='gower')
d_matrix3 = daisy(size3, metric='gower')
d_matrix4 = daisy(size4, metric='gower')

h = hclust(d_matrix4)
plot(h)

k0=data.frame()
for(i in 2:20){
  k2 = kmeans(size1[,-1], i, iter.max=10000)
  k3 = cbind(i, k2$tot.withinss)
  k0 = rbind(k0,k3)
}
plot(k0, type='b')


k = kmeans(z_data, centers=11, iter.max = 10000)
k$withinss
k$tot.withinss
str(k$cluster)

data2 = data1
data2 = data2[which(data2$valueBysize1!=Inf),]
View(data2)
data2$label = k$cluster

str(data2$label)

for(i in length(unique(data2$label))){
  tmp = data.frame()
  for(z in nrow(data2)){
    if(data2$label[z]==i){
      tmp = rbind(tmp, data2[z])
    }
  }
  result = data.frame()
  for(i in 6:19){
    result= c(result, tmp[i]/sum(tmp[i]))
  }

}

install.packages('factoextra')
library(factoextra)
res.dist = get_dist(size4[,-1], stand = TRUE)
a = fviz_dist(res.dist, gradient = list(low='#00AFBB', mid='white', high = '#FC4E07')) 
b = fviz_nbclust(size4[,-1], kmeans, k.max = 20, method = 'gap_stat')

library(NbClust)
res.nbclust = NbClust(size4[,-1], min.nc=2, max.nc=20, method = "complete")
fviz_nbclust(res.nbclust)

res.hc = eclust(size4[,-1], "hclust", k=3, graph=FALSE)
fviz_dend(res.hc, rect=TRUE, show_labels = FALSE)

View(cor(size1[,-1]))
plot(size1[,-1])
pca_size1 = prcomp(size1[,-1])
summary(pca_size1)
print(pca_size1)
str(pca_size1)
plot(pca_size1, type='l')

PRC = as.matrix(size1[,-1]) %*% pca_size1$rotation
head(PRC)
View(PRC)

k0=data.frame()
for(i in 2:20){
  k2 = kmeans(PRC[,1:8], i, iter.max=10000)
  k3 = cbind(i, k2$tot.withinss)
  k0 = rbind(k0,k3)
}
plot(k0, type='b')

size1_dist = get_dist(PRC[,1:8], stand=TRUE)
a = fviz_dist(size1_dist, gradient = list(low='#00AFBB', mid='white', high = '#FC4E07'))
b = fviz_nbclust(PRC[,1:8], kmeans, k.max = 20, method = 'gap_stat') #최적 군집 수 : 5개
size1_nbclust = NbClust(PRC[,1:8], min.nc=2, max.nc=20, method = "complete") #3개

