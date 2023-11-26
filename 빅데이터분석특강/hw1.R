data <- read.csv("표2-5범죄관련데이터.csv")
str(data)


## 1
attach(data)
data_pre <- data[, -c(1:3)] # 수치형 변수가 아닌 것을 제거
# (a)
library(lattice)
splom(data_pre) 

# (b)
library(aplpack)
faces(data_pre) 

# (c)
stars(data_pre)

# (d)
mean(arson); var(arson)


## 2

# (a)
c(summary(arson)[c(4, 3, 6, 1)], quantile(arson, 0.9))

# (b)
dim(subset(data, murder >= 10))[1] # 다른 방법  

sum <- 0
for (i in 1:dim(data)[1]){
  if (data$murder[i] >= 10) {
    sum = sum + 1
  }
}
sum # for, if 구문을 활용한 방법법


# (c)
hist(arson, nclass = 10)


## 3 

# (a)
par(mfrow = c(1, 2))
plot(murder ~ robbery)
plot(motor.vehicle.theft, larceny.theft)

# (b)
par(mfrow = c(1, 1))
boxplot(murder ~ state, main = "state에 따른 murder") # 하나의 데이터만 있을 경우 선 하나로 표시된다. 

# (c)
cloud(forcible.rape ~ aggravated.assault * burglary, main = "구름 삼차원 산점도")

