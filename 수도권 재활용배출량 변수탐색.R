##====================사용할 패키지 불러오기====================
library(dplyr)
library(car)

##====================작업경로 지정====================
setwd('/Users/yangjaeyeong/Desktop/공빅데 프로젝트/수도권 Data')

##====================데이터 불러오기==================== 

# Python을 통해 작업한 데이터 불러오기
model_data <- read.csv('model_data.csv', fileEncoding = 'CP949')
head(model_data)

# '폐기물관리손수레_구간' 변수의 경우 범주형이기 때문에 변환해주도록 한다.
model_data$폐기물관리손수레_구간 <- as.factor(model_data$폐기물관리손수레_구간)

# 단계적 선택 방법을 사용하여 변수 선택
step_model <- step(lm(대체_재활용 ~ 1, data = model_data), 
                   scope = list(lower = ~1, upper = ~인구_주성분 + 도시지역면적 + 주거지역면적 + 상업지역면적 + 폐기물관리손수레_구간),
                   direction = 'both')

# 단계적 선택 방법을 통해 '인구_주성분', '주거지역면적' 변수 선택
model <- lm(대체_재활용 ~ 인구_주성분 + 주거지역면적, data = model_data)
summary(model)
