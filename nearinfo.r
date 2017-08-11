

View(data)



data = read.csv('Project/include_lonlat/경기도 동두천.csv', header=T, encoding = 'euc-kr')

str(data)
sub <- data[!is.na(data$재산면적),c(4,16,17,19,32,33)]
head(sub)
sub <- sub[sub$실지목명  != '도로' & sub$공부지목명 != '도로',]
sub <- sub[sub$실지목명  != '하천' & sub$공부지목명 != '하천',]
head(sub)
lon = sub[,5]
lat = sub[,6]
head(sub2) ; str(sub2)



source('Project/using_kakaoapi.R')
library(ggmap)
result = list()
#place_name, x, y, distance

start = Sys.time()
for(i in 1:nrow(sub)){
  tmp=list()
  for(z in 1:length(categories)){
    tmp[[z]] = nearBySearch(categories[z],lon[i],lat[i])
    
  }
  
  result[[i]] = tmp
}
print(Sys.time()-start)
length(result[[1]][[1]])






a =numeric(20)

