### 코드 ###　

## 1
data <- read.csv("표2-5범죄관련데이터.csv")
attach(data)

# (a)
par(mfrow = c(1, 2))
plot(murder ~ robbery)
plot(motor.vehicle.theft, larceny.theft)

# (b)
par(mfrow = c(1, 1))
boxplot(murder ~ state, main = "state에 따른 murder") # 하나의 데이터만 있을 경우 선 하나로 표시된다. 

# (c)
library(lattice)
cloud(forcible.rape ~ aggravated.assault * burglary, main = "구름 삼차원 산점도")


## 2

# 통계학및실습
# r프로그램 실습 (과제2)풀이 수행.

# 범죄관련데이터 대신 ‘Boston’ 주택가격 자료 이용

  boston <- read.csv("boston_hw.csv")
  head(boston)
  str(boston)
  
  # CRIM  = 도시별 범죄율
  # ZN    = 주택당 면적 비율
  # INDUS = 비상업 건물비율
  # CHAS  = 강가 여부
  # NOX   = 산화질소 농도
  # RM    = 평균 방의 수
  # AGE   = 건물 연식
  # DIS   = 5개의 고용중심가 평균거리
  # RAD   = 고속도로 접근성 지수
  # TAX   = $10,000당 재산 세율
  # PTRATIO = 도시별 학생-교사 비율
  # B       = 흑인인구 비율
  # LSTAT   = 저소득층 비율
  # MEDV    = $1000 당 주택 중앙 값
  
  # 미국 보스턴 지역의 주택 가격에 영향을 미치는 요소들을 정리함
  
  
  ## 2
  
  for (i in 1:dim(boston)[2]) {
    print(sum(is.na(boston[i])))} # 결측값의 대략적인 분포를 알 수 있다.
  
  # (a)
  fn1 <- function(data){ # 최빈값으로 값을 대체하는 함수
    data_new <- data
    temp <- names(which.max(table(data)))
    
    data_new[is.na(data)] <- temp
    return(data_new)
  }
  
  fn2 <- function(data, cols){ # 중앙값으로 값을 대체하는 함수 
    data_new <- data
    
    for (i in cols){  
    temp <- data_new[, i]
    temp[is.na(temp)] <- median(temp, na.rm = T)
    data_new[, i] <- temp
    }
    return(data_new)
  }

boston_new <- boston

boston_new$CHAS <- fn1(boston_new$CHAS)

boston_new <- fn2(boston_new, c(6, 7, 9, 12, 14)) # 중앙값을 대체할 열을 선택해서 집어넣음.


for (i in 1:dim(boston)[2]) {
  print(sum(is.na(boston_new[i])))} # 결측값이 존재하지 않는 것을 확인할 수 있다. 

# (b)

fn3 <- function(data){ # min_max scaler
  d_min <- min(data)
  d_max <- max(data)
  
  return((data - d_min)/(d_max - d_min))
}

boston_new$LSTAT <- fn3(boston_new$LSTAT)
head(boston$LSTAT) # 차이가 있는 것을 확인할 수 있다. 
head(boston_new$LSTAT)

# (c)
AGE1 <- c()
AGE <- boston_new$AGE
range(AGE) # 2.9 ~ 100

for (i in 1:dim(boston_new)[1]){
  AGE1[i] <- ifelse(AGE[i] < 20, 1, 
                    ifelse(AGE[i] < 40, 2, 
                           ifelse(AGE[i] < 60, 3,
                                  ifelse(AGE[i] < 80, 4, 5))))
}

boston_new$AGE1 <- AGE1
head(boston_new)
head(boston) # 적절하게 추가된 것을 확인할 수 있다. 


# (d)
library(plyr) # ddply를 사용하기 위해 필요한 패키지
ddply(boston_new, .(AGE1), summarise, MEDV_mean = mean(MEDV), MEDV_sd = sd(MEDV)) ## 그룹별 요약 

## 3 

# (a)
rm(list = ls())
data <- read.csv("표2-5범죄관련데이터.csv")
attach(data)

library(SemiPar)
fit <- spm(larceny.theft ~ f(motor.vehicle.theft))
plot(fit)

# (b)

plot(fit, col = "green", lwd = 5,  shade.col = "mediumpurple1", rug.col = "blue",
     main = "Semiparametric regression model", ylab = "larceny.theft")
points(motor.vehicle.theft, larceny.theft, col = "orange", pch = 23)

