# 3차 과제 -------------------------------------------------------------------216803 박주성


# 자료실에 올려진 한국 강수량 자료에 대해 다음과 같은 R program 작업을 실행합니다. 

# 구해야 할 통계량의 정의:

# TP= 1년 총강수량.  AMP1=1년 중 최대 1일 강수량. AMP5=1년 중 최대 5일 강수량.

# WD=강수 일수 (1일 0.2mm 이상 비온 날 수). CWD =연속 강수 일수. DD=무강수 일수.   CDD=연속 무강수 일수


# 1-1) 데이터 경로 설정

# 1-2) 데이터 읽기

data <- read.csv("daily_korea_precipitation_selected2.csv")
infor <- read.csv("META_OBS_Infor.csv")

# 1-3) 데이터 확인

head(data) 
tail(data)

# Station_No : 관측지점번호, Date : 일자, Precipitation : 강수량 로 구성되어 있고
# 무강수 일자는 빠진 것을 알 수 있다.
#  ==> AMP5, CWD, DD, CDD를 구하기 위해서는 자료에 무강수 자료를 포함시켜줘야 한다.
#  ==> 관측기간은 1980년 1월 1일 부터 2019년 12월 31일이다.

# 먼저 편의를 위해 변수명을 요약해서 다시 적음

names(data) <- c("stn","date","prec")
data$date <- as.Date(data$date)     # 날짜형태로 변환 

# 1-4) 무강수 자료를 포함시켜주기 위해 모든 관측기간 일 단위 일자 변수를 생성한다.
# 연도별로 TP, AMP1을 구해야 하기 때문에, 연도 변수를 만들어 준다. 

library(plyr)
date <- seq.Date(from = as.Date("1980-01-01"), to = as.Date("2019-12-31"), by = "day")
stn <- unique(data$stn); length(stn) # 관측지점수 # 60

data1 <- list()

for(s in 1:length(stn)){
  
  data1[[s]] <- data.frame(stn = stn[s], date = date, year = format(date, "%Y")) # date의 갯수에 depend 된다.(365?)
  data1[[s]] <- join(data1[[s]], data, by=c("stn", "date")) # 강수가 없는 경우 NA로 채워질 것이다. 
  
  data1[[s]]$prec[is.na(data1[[s]]$prec)] <- 0 #NA값 0으로 변환
  # print(s)
}

head(data)
head(data1[[1]]) # 예시 90번째 지점


# 1-5) 다시 하나의 파일들어서 구해도 되고, list 파일을 이용해도 된다.

library(data.table)

data1a <- data.frame(rbindlist(data1)); head(data1a) #데이터 하나의 파일로 합치기

sum(is.na(data1a$prec)) # NA 여부 확인
sum(data1a$prec < 0)      # 음수 여부 확인

# 전처리 완료

write.csv(data1a, "data_modify.csv", row.names = F)


# 1번 문제 -------------------------------------------------------------------

## 각 지점에 대해 TP, AMP1 을 연도별로 구한 다음, 
## 이를 바탕으로 40년간를의 min, Q1, Q2, Q3, max, mean 을 구하여 표 만들어 보여주어라. (지점 이름 포함)

## Hint1 각 지점에 대해 연도별로 TP, AMP1을 구하는 방법 ==> ddply 함수를 이용
## Hint2 Q1,Q2,Q3 구하는 방법 ==> quantile 함수 이용

data1a <- read.csv("data_modify.csv", header = T)
library(plyr)
library(data.table)

u_stn <- unique(data1a$stn)
u_year <- unique(data1a$year)
n_stn <- length(unique(data1a$stn))
n_year <- length(unique(data1a$year))

data_TP <- ddply(data1a, c("stn", "year"), summarize, TP = sum(prec))
data_AMP1 <- ddply(data1a, c("stn", "year"), summarise, AMP1 = max(prec))

data_TP <- join(data_TP, infor, by = "stn")
data_AMP1 <- join(data_AMP1, infor, by = "stn")

head(data_TP)
head(data_AMP1)

fn1 <- function(data){ # data summary
  Q1 <- quantile(data, 0.25) ; names(Q1) <- NULL
  Q3 = quantile(data, 0.75) ; names(Q3) <- NULL
  
  temp <- c(min = min(data), Q1 = Q1, Q2 = median(data), Q3 = Q3, max = max(data), mean = mean(data))
  
  return(temp)
}

# fn2 <- function(data, c){ # 요약값과 가장 근접한 값의 인덱스를 찾기 위한 함수(현재 필요하지 않는 함수)
#   temp <- data[1]
#   k <- 1
#   for (i in 2:length(data)){
#     if (abs(temp - c) > abs(data[i] - c)){
#       temp <- data[i]
#       k <- i
#     }
#   }
#   
#   return(k)
# }
# 

fn2 <- function(data1, data2){ # 하나로 합치는 함수 
  temp <- matrix(0, ncol = 6)
  colnames(temp) <- c("min", "Q1", "Q2", "Q3", "max", "mean")
  
  for (i in u_stn){
    temp <- rbind(temp, matrix(fn1(data1[data2$stn == i]), nrow = 1))
  }
  
  
  u_stn1 <- infor[, 1:2]
  temp <- data.frame(temp[-1, ])
  temp <- cbind(u_stn1, temp)
  
  return(temp)
}

TP_s <- fn2(data_TP$TP, data_TP)
TP_s # 요약 1

AMP1_s <- fn2(data_AMP1$AMP1, data_AMP1)
AMP1_s # 요약 2



# 2번 문제 -------------------------------------------------------------------

##  각 지점에 대해  연도별로 5일 이동합계을 구한 뒤, 그 중 최대값을 AMP5 라고 한다.   
##  이 AMP5 에 대해서도 #1 과 같은 표를 작성하여라.

##  Hint3 : 각 지점, 연도별, 5일 이동합계 구하는 방법 ==> rollapply 함수 이용
##  지점별로 구하는 방법 list 를 이용


library(zoo)

data_AMP5 <- ddply(data1a, c("stn", "year"), summarise, AMP5 = max(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right"), na.rm = T))

data_AMP5 <- join(data_AMP5, infor, by = "stn")
head(data_AMP5)

AMP5_s <- fn2(data_AMP5$AMP5, data_AMP5)
AMP5_s # 요약 3


# 3번 문제 -------------------------------------------------------------------


## 각 지점에 대해  연도별로 WD 를 구하라. 이 통계량에 대해서도 #1 과 같은 표를 작성하여라.

data_WD <- ddply(data1a, c("stn", "year"), summarise, WD = sum(prec >= 0.2))
head(data_WD)

WD_s <- fn2(data_WD$WD, data_WD)
WD_s # 요약 4


# 4번 문제 -------------------------------------------------------------------

data_TP$year <- as.numeric(data_TP$year)
data_AMP1$year <-as.numeric(data_AMP1$year)
data_AMP5$year <-as.numeric(data_AMP5$year)
data_WD$year <- as.numeric(data_WD$year)


draw_plot <- function(data, c = 1){ # 전체데이터를 그린다. 
  for (i in u_stn){
    data_p <- data[data$stn == i, ][, c(2:4)]
    plot(data_p[, 1], data_p[, 2],type= "o", pch = 15, ylab = colnames(data_p)[2], xlab = "year", main = data_p[1, 3], las = 1, col = c)
  }
}

draw_line <-  function(data, c = 2){ # 전체데이터를 그린다. 중복해서(TP, AMP1, AMP5, WD)
  for (i in u_stn){
    data_p <- data[data$stn == i, ][, c(2:4)]
    lines(data_p[, 1], data_p[, 2],type= "o", pch = 15, ylab = colnames(data_p)[2], xlab = "year", main = data_p[1, 3], las = 1, col = c)
  }
}

draw_plot2 <- function(data1, data2, data3, data4, c = c(1, 2, 3, 4)){ # 스케일이 맞지 않는 통계량끼리는 사용하기에 어려움이 있는 fn.
  for (i in u_stn){
    data_p1 <- data1[data1$stn == i, ][, c(2:4)]
    data_p2 <- data2[data2$stn == i, ][, c(2:4)]
    data_p3 <- data3[data3$stn == i, ][, c(2:4)]
    data_p4 <- data4[data4$stn == i, ][, c(2:4)]
    
    draw_plot(data1, c[1])
    draw_line(data2, c[2])
    draw_line(data3, c[3])
    draw_line(data4, c[4])
  }
}

draw_box <- function(data){
  for (i in u_stn){
    data_p <- data[data$stn == i, ][, c(2:4)]
    boxplot(data_p[, 2], names = colnames(data_p)[2], main = data_p[1, 3], las = 1)
  }
}


par(mfrow = c(3, 4))

draw_plot(data_TP)
draw_plot(data_AMP1)
draw_plot(data_AMP5)
draw_plot(data_WD)

# draw_box(data_TP) # 특징을 알아보기에 시계열보다 어렵다. 


# 위아래로 흔들림이 크고, 그의 빈도 수가 많고, 뾰족하고 등... 개인적인 판단이 많을 것이라 생각됨.
# 울진, 대전, 인제, 제천, 금산, 구미 (130, 133, 211, 221, 238, 279)

fn3 <- function(city){ # 그림에 그릴 데이터 추출 및 병합용 함수 
  idx <- data_TP$name == city
  
  d1 <- data_TP[, 2:4]; d1 <- d1[idx, ]
  # d2 <- data_AMP1[, 2:4]; d2 <- d2[idx, ]
  # d3 <- data_AMP5[, 2:4]; d3 <- d3[idx, ]
  # d4 <- data_WD[, 2:4]; d4 <- d4[idx, ]
  
  # d5 <- join(join(d1, d2), join(d3, d4))[, c(1, 3, 2, 4:6)]
  
  return(d1)
}

x <- c("울산", "대전", "인제", "제천", "금산", "구미")
x_list <- list()

for (i in 1:6){
  x_list[[i]] <- fn3(x[i])
}

head(x_list[[1]])


# 최종
par(mfrow = c(2, 3))

for (i in 1:6){
  plot(x_list[[i]][, 1], x_list[[i]][, 2], type= "o", pch = 15, ylab = colnames(x_list[[i]])[2], xlab = "year", main = x_list[[i]][1, 3], las = 1)
}
