## required packages
library(data.table)
library(tidyverse)
library(cluster)
library(mclust)
library(ClusterR)
library(fpc)
library(NbClust)
library(factoextra)

## 0. data loading

raw_data <- read.csv(file = "파일경로", header = T, fileEncoding = 'CP949')
head(raw_data)
str(raw_data)

data_clust1 = raw_data %>% select(변수1, 변수2, 변수3)
data_clust1 = data_clust1 %>% scale() %>% as.data.frame()

## 1. 계층적 군집분석

clust1 = eclust(data_clust1, 'hclust', hc_method = 'average', k=3)
hclust_result = clust1$cluster

fviz_cluster(clust1) + 
  theme(panel.background = element_rect(fill = 'white', color = 'black', linetype='solid')) + 
  labs(x = 'population', y = "y", title = '')

clust1

## 2. K-means

km <- eclust(data_clust1,'kmeans', k=4, nstart=25)
fviz_silhouette(km)

km

plot(silhouette(kmeans$cluster, dist = dist(data_clust2)), col=1:3)  # 실루엣

## 3. K-medoids
kmed <- pam(data_clust1, k=4)
fviz_cluster(kmed, data = data_clust1)

kmed

## 4. DBSCAN
db <- fpc::dbscan(data_clust1, eps=sqrt(1), MinPts = 3)  #eps와 MinPts 다시 조정해야함
fviz_cluster(db, data_clust1, stand=T, geom = "point")

db


####### add cluster assignment to original data
final_data <- cbind(USArrests, cluster = kmed$cluster)

#view final data
head(final_data)