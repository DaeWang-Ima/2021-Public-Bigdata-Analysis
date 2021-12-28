##================================작업환경 설정================================
setwd('/Users/yangjaeyeong/Desktop/공공빅데이터 청년인턴십/p_median data')

# P-Median을 사용하기 위한 패키지 설치
#install.packages("tbart")
#install.packages("geosphere")
#install.packages("data.table")
#install.packages("tidyverse")

library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지rary(data.table)
library(data.table)
library(tidyverse)

##================================함수 정의================================

# 거리계산 행렬을 먼저 생성
make_dis_mat <- function(lon, lat, p_medain) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n) # 거리계산 행렬을 만들어 주기 위해 초기 행렬 저장
  
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    
    for (j in 1:n) {
      lon2 <- lon[j]
      lat2 <- lat[j]
      
      # Haversine 공식을 사용하여 초기 행렬에 저장
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  }
  return(dist_mat)
}

p_median <- function(distance_matrix, num_p, seed) {
  
  # 수행할때마다 결과가 달라지는 것을 방지하기 위해 Seed Number 지정
  set.seed(seed)
  
  # 초기값을 랜덤으로 지정한다.
  initial_vlaue <- sample(1:nrow(distance_matrix), num_p)
  
  # 랜덤으로 지정된 개수를 가지고 P-Median 알고리즘 적용
  result_vec <- tb.raw(d = distance_matrix, guess = initial_vlaue, verbose = T)
  
  # P-Median 알고리즘 적용 후, 선정된 행과열을 제외하고 0을 대입
  distance_matrix[-result_vec, -result_vec] <- 0
  
  # P-Median 알고리즘 적용 후, 선정된 행에 0을 대입
  distance_matrix[result_vec, ] <- 0
  
  for_vec <- 1:nrow(distance_matrix)
  for_vec <- for_vec %>% setdiff(result_vec)
  
  for (i in for_vec) {
    a <- distance_matrix[i, result_vec]
    a[which(a != distance_matrix[i, result_vec] %>% min)] <- 0 # 최단거리만 저장
    
    distance_matrix[i, result_vec] <- a
  }
  return(distance_matrix)
}

##================================작업환경 설정================================

# 아파트를 제외한 데이터 불러오기
apt <- read.csv('아파트 제외 데이터.csv', fileEncoding = 'CP949')
head(apt)

##==========서교동==========
sugyo <- apt %>% filter(행정동 == '서교동')

# 서교동 데이터의 경도, 위도를 Vector로 저장
sugyo_lon <- sugyo %>% select(경도) %>% unlist() %>% as.vector()
sugyo_lat <- sugyo %>% select(위도) %>% unlist() %>% as.vector()

# 서교동 건물좌표를 기반으로 거리행렬 계산
sugyo_distmat <- make_dis_mat(sugyo_lon, sugyo_lat)

## P-Median 알고리즘 중심을 변경해가면서 적용
sugyo_1 = p_median(sugyo_distmat, 1, seed = 1000)
# Configuration:  91  Score: 116510.7 

sugyo_2 = p_median(sugyo_distmat, 2, seed = 1000)
# Configuration:  16 90  Score: 75311.68

sugyo_3 = p_median(sugyo_distmat, 3, seed = 1000)
# Configuration:  21 143 154  Score: 55588.88

sugyo_4 = p_median(sugyo_distmat, 4, seed = 1000)
# Configuration:  21 153 147 75  Score: 47981.83

sugyo_5 = p_median(sugyo_distmat, 5, seed = 1000)
# Configuration:  21 155 41 75 178  Score: 39746.96

sugyo_6 = p_median(sugyo_distmat, 6, seed = 1000)
# Configuration:  28 48 155 75 178 41  Score: 35181.38

sugyo_7 = p_median(sugyo_distmat, 7, seed = 1000)
# Configuration:  28 64 75 169 178 203 9  Score: 31017.41

sugyo_8 = p_median(sugyo_distmat, 8, seed = 1000)
# Configuration:  28 64 75 181 178 203 9 133  Score: 27793.71


plot_data <- data.frame(x = c(1:8), Score = c(116510.7, 75311.68, 55588.88, 47981.83, 39746.96, 35181.38, 31017.41, 27793.71))

ggplot(data = plot_data, aes(x = x, y = Score)) +
  geom_line() +
  geom_point() +
  xlab('') + 
  ylab('')