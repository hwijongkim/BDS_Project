library(dplyr)

# Data cleaning to get rid of records with no geo info
# Leaflet bindings are a bit slow; for now we'll just sample to compensate (10000 samples)
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############
# vc <- read.csv("data/NYPD_Motor_Vehicle_Collisions.csv") # 618,358 observations
# vc <- subset(vc,!(is.na(vc['ZIP.CODE']))) # 470,687 observations
# set.seed(100)
# vc <- vc[sample.int(nrow(vc), 10000),]
# write.csv(vc, file = 'data/NYPD_Motor_Vehicle_Collisions (10000 obs).csv')
###################################

# Import truncate data (vc is short for vehicle collision)

vc <- read.csv('Project/시각화 코드/data/Namyangju.csv')
vc <- vc[sample(nrow(vc),100),]
vc$avg_price = (vc$대장가액.원./vc$재산면적)
vc$대장가액.원. <- ifelse(is.na(vc$대장가액.원.), 0, vc$대장가액.원.)
vc <- data.frame(vc$latitude, vc$longitude, vc$공부지목명, vc$재산면적, vc$대장가액.원., vc$avg_price)
colnames(vc) <- c('lat', 'lon', 'assets_name', 'size', 'tot_price', 'avg_price')
vc$size <- ifelse(is.na(vc$size), 1, vc$size)
vc$avg_price <- ifelse(is.na(vc$avg_price), 0, vc$avg_price)
vc$avg_price <- ifelse(vc$avg_price == "Inf", 1, vc$avg_price)
vc$avg_price <- ifelse(vc$avg_price > 5000000, 1, vc$avg_price)
vc$avg_price = round(vc$avg_price)
vc$assets_name <- as.character(vc$assets_name)
vc$popup <- paste("assets_name: ", vc$assets_name, " size: ", vc$size, 
                  " tot_price: ", vc$tot_price, " avg_price: ", vc$avg_price)

