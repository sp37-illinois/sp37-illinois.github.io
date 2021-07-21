#install.packages("tidyverse")
library(tidyr)
#install.packages('dplyr') 
library(dplyr) 
library(ggplot2)
library(reshape2)

# ������ �ε�
m2 <- read.csv('m2.csv', header=T, fileEncoding = 'euc-kr', encoding = 'utf-8') 
real_estate <- read.csv('real_estate_monthly.csv', header=T, fileEncoding = 'euc-kr', encoding = 'utf-8') 

# ��¥ Ÿ������ �ٲ�ٰ� Year, month day �и�
m2 = separate(m2, "����", c("Year", "Month"), sep = ". ", remove=FALSE)
m2 = m2[, !(names(m2) %in% c("����"))]

#real_estate$Classification = as.Date(with(real_estate, Classification), format="%Y-%m-%d")
#real_estate = separate(real_estate, "Classification", c("Year", "Month", "Day"), sep = "-", remove=FALSE)
real_estate = separate(real_estate, "Classification", c("Year", "Month"), sep = ". ", remove=FALSE)

# ������ �����ؼ� �ϳ��� �����
data = left_join(real_estate, m2, by = c("Year" = "Year", "Month" = "Month"))

# �� Į������ �� ����� pred�̾� ����
columns = colnames(data)
columns = columns[!(columns %in% c("Classification", "Year", "Month", "M2", "M2_index"))] # TODO: ���߿� �ѱ۷� �ٲٱ�
for (target in columns){
  tryCatch({
    #print(target)
    model = lm(paste(target, "~ I(M2_index ^ 2) + M2_index"), data = data)
    data[paste(target, "_pred", sep="")] = predict(model, newdata = data)
  }, error=function(e){cat("Error in processing(", target, "): ", conditionMessage(e), "\n")})
}

write.csv(data, "final_data.csv", na="", row.names = FALSE)



# ������. �ϳ��ϳ� R2 ������
target = "Total"
model = lm(paste(target, "~ I(M2_index ^ 2) + M2_index"), data = data[1:412,])
#model = lm(paste(target, "~ M2^2 + M2"), data = data[1:412,])
sprintf("%.20f", coef(model))
summary(model)

