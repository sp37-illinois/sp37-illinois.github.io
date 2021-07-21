#install.packages("tidyverse")
library(tidyr)
#install.packages('dplyr') 
library(dplyr) 
library(ggplot2)
library(reshape2)

# 데이터 로드
m2 <- read.csv('m2.csv', header=T, fileEncoding = 'euc-kr', encoding = 'utf-8') 
real_estate <- read.csv('real_estate_monthly.csv', header=T, fileEncoding = 'euc-kr', encoding = 'utf-8') 

# 날짜 타입으로 바꿨다가 Year, month day 분리
m2 = separate(m2, "시점", c("Year", "Month"), sep = ". ", remove=FALSE)
m2 = m2[, !(names(m2) %in% c("시점"))]

#real_estate$Classification = as.Date(with(real_estate, Classification), format="%Y-%m-%d")
#real_estate = separate(real_estate, "Classification", c("Year", "Month", "Day"), sep = "-", remove=FALSE)
real_estate = separate(real_estate, "Classification", c("Year", "Month"), sep = ". ", remove=FALSE)

# 데이터 조인해서 하나로 만들기
data = left_join(real_estate, m2, by = c("Year" = "Year", "Month" = "Month"))

# 각 칼럼마다 모델 만들고 pred뽑아 저장
columns = colnames(data)
columns = columns[!(columns %in% c("Classification", "Year", "Month", "M2", "M2_index"))] # TODO: 나중에 한글로 바꾸기
for (target in columns){
  tryCatch({
    #print(target)
    model = lm(paste(target, "~ I(M2_index ^ 2) + M2_index"), data = data)
    data[paste(target, "_pred", sep="")] = predict(model, newdata = data)
  }, error=function(e){cat("Error in processing(", target, "): ", conditionMessage(e), "\n")})
}

write.csv(data, "final_data.csv", na="", row.names = FALSE)



# 번외편. 하나하나 R2 봐보기
target = "Total"
model = lm(paste(target, "~ I(M2_index ^ 2) + M2_index"), data = data[1:412,])
#model = lm(paste(target, "~ M2^2 + M2"), data = data[1:412,])
sprintf("%.20f", coef(model))
summary(model)

