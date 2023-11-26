# 5차 과제 -------------------------------------------------------------------216803 박주성
library(plyr)
library(data.table)
library(zoo)
library(ggplot2)
library(ggmap)
library(lubridate) # 오류가 발생해서 사용하지 않았습니다. 
library(ggridges)
library(trend) # mk.test, pettitt.test 를 위한 패키지 

## 0 데이터 준비 및 사용할 함수 정의
data <- read.csv("data_modify.csv", header = T)
infor <- read.csv("META_OBS_Infor.csv", header = T)

u_stn <- unique(data$stn)
u_year <- unique(data$year)
n_stn <- length(unique(data$stn))
n_year <- length(unique(data$year))


## 1
#  각 지점별로, 월별 강수량에 대해 ridge line plot 을 그려라.  
# 그림이 많을테니 특별한 관심이 가는 곳 6군데만 제출하세요.

# 데이터를 그릴 수 있게 쪼개는 과정이 필요하고 이를 연속적으로 할 수 있게 만들어야 한다.
idx_fn1 <- function(idx){
  if (idx < 20){
    return(idx + 1)
  }
  else{
    cat("stop!, idx is max \n")
    return(NA)
  }
}

# 순서가 조금 섞이는데 이는 한국어의 순서 때문이다.
draw_ridge <- function(data, idx, stn1){
  temp <- subset(data, stn == stn1[1] | stn == stn1[2] | stn == stn1[3])
  title <- paste(idx, "")
  
  g <- ggplot(temp, aes(x = M_P, y = m, fill = m))
  g + geom_density_ridges() + facet_wrap(~name) + labs(title = title, x = "월강수량", y = "month", fill = "month")
}

data_new <- data
data_new$date <- as.Date(data_new$date)
data_new$ym <- format(data_new$date, "%Y-%m")
data_new$m <- format(data_new$date, "%m")
data_new <- data_new[, c(1, 5, 6, 4)]
data_new$m <- factor(data_new$m)
# head(data_new)
# str(data_new)

M_P <- ddply(data_new, c("stn", "ym", "m"), summarise, M_P = sum(prec)) # 지점에 따른 월별 강수량 
M_P <- join(M_P, infor)


idx <- 1
stn1 <- u_stn[(3*idx - 2):(3*idx)]
draw_ridge(M_P, idx, stn1)

######## 오류가 발생할 때까지 출력하면 전체 데이터에 대해서 그림을 출력한 것
idx <- idx_fn1(idx)
stn1 <- u_stn[(3*idx - 2):(3*idx)]
draw_ridge(M_P, idx, stn1)
########  HW4와 비슷하게 ggplot이 반복문과 같이 실행되지 않아서 이렇게 구성합니다. 

# 울릉도, 전주, 제주, 정읍, 남원, 산청 

x <-  c("울릉도", "전주", "제주", "정읍", "남원", "산청")
x_stn <- c()

for (i in x){
  x_stn <- c(x_stn, infor$stn[infor$name == i])
}

draw_ridge(M_P, "# 1", x_stn[1:3]) # ans
draw_ridge(M_P, "# 2", x_stn[4:6])


## 2 
# AMP1, AMP5, CWD max, CDD max, TP 에 대해 1980년대, 1990년대, 2000년대, 2010년대 평균을 네 개의 한국 지도로 그려라.
# 10년대별 시기에 따른 변화를 눈으로 보고자 한다. 

# 데이터 구성을 위한 함수 구성 

CWD <- function(x){
  num <- integer(length(x))
  num[1] <- ifelse(x[1] >= 0.2, 1, 0) # 0.2mm 이상 온 것을 비가 온 것으로 정의함.
  
  for(i in 2:length(x)){
    if (x[i] < 0.2){
      num[i] <- 0
    } else {
      num[i] <- num[i - 1] + 1
    }
  } 
  return(num)
}

CDD <- function(x){
  num <- integer(length(x))
  num[1] <- ifelse(x[1] < 0.2, 1, 0) 
  
  for(i in 2:length(x)){
    if (x[i] >= 0.2){
      num[i] <- 0
    } else {
      num[i] <- num[i - 1] + 1
    }
  }
  return(num)
}


fn_CWD <- function(data){ # data summary
  temp <- ddply(data, c("stn", "year"), summarise, max = max(CWD))
  
  return(temp)
}

fn_CDD <- function(data){ # data summary
  temp <- ddply(data, c("stn", "year"), summarise, max = max(CDD))
  
  return(temp)
}


idx_fn2 <- function(idx){
  temp <- idx
  
  temp[2] <- temp[2] + 10
  
  if (temp[2] > 2010){
    temp[1] <- temp[1] + 1
    temp[2] <- 1980
  }
  
  if (temp[1] >= 8){
    cat("stop!, idx is max \n")
    return(NA)
  }
  else {
    return(temp)
  }
  
}


fn1 <- function(data, title){ # 그림을 그리기 위한 함수 
  map <- map_data("world2", region = "South Korea")
  
  ggplot(map) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "#87CEFA") +
    geom_point(data = data, aes(x = long, y = lat, color = data[, 2], size = data[, 2])) +
    scale_color_gradient("mean", low = "blue", high = "red") +
    guides(color = guide_colorbar("mean"), size = "none") + 
    geom_text(data = data, aes(x = long + 0.1, y = lat + 0.1, label = name), 
              size = 6, color = "black", alpha = 0.6) + labs(title = title, fill = "")
  
}


draw_map <- function(data, idx){
  temp <- join(subset(data, decade == idx[2])[, c(1, idx[1])], infor)
  title <- paste(idx[2], "'s ", names(temp)[2], "_mean", sep = "")
  
  fn1(temp, title)
}


data_CWD <- ddply(data, c("stn", "year"), summarise, CWD = CWD(prec))
data_CDD <- ddply(data, c("stn", "year"), summarise, CDD = CDD(prec))

data_AMP1 <- ddply(data, c("stn", "year"), summarise, AMP1 = max(prec))
data_AMP5 <- ddply(data, c("stn", "year"), summarise, AMP5 = max(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right"), na.rm = T))
CWD_m <- fn_CWD(data_CWD)
CDD_m <- fn_CDD(data_CDD)
data_TP <- ddply(data, c("stn", "year"), summarize, TP = sum(prec))

decade <- rep(c(1980, 1990, 2000, 2010), each = 10, times = 60) # n_stn = 60 이므로 이와 같이 구성한다. 

data_AMP1$decade <- decade
data_AMP5$decade <- decade
CWD_m$decade <- decade
CDD_m$decade <- decade
data_TP$decade <- decade

# 사용할 데이터 형성 
AMP1_mean <- ddply(data_AMP1, c("stn", "decade"), summarise, AMP1 = mean(AMP1)) 
AMP5_mean <- ddply(data_AMP5, c("stn", "decade"), summarise, AMP5 = mean(AMP5))
CWD_mean <- ddply(CWD_m, c("stn", "decade"), summarise, CWD = mean(max))
CDD_mean <- ddply(CDD_m, c("stn", "decade"), summarise, CDD = mean(max))
TP_mean <- ddply(data_TP, c("stn", "decade"), summarise, TP = mean(TP))
  
d1 <- join(AMP1_mean, join(AMP5_mean, join(CWD_mean, join(CDD_mean, TP_mean))))
# head(d1)

idx2 <- c(3, 1980) # 초기상태 
draw_map(d1, idx2)
ggsave(paste(idx2[2], "'s ", names(d1)[idx2[1]], "_mean.png", sep = ""), 
       last_plot(), width = 5, height = 5, dpi = 400) # 그림저장


##################################################### 오류가 발생할 때까지 반복한다. 
idx2 <- idx_fn2(idx2)
draw_map(d1, idx2)
ggsave(paste(idx2[2], "'s ", names(d1)[idx2[1]], "_mean.png", sep = ""), 
       last_plot(), width = 5, height = 5, dpi = 400)
###########################################################################



## 3 
# (통계학 및 컴공 전공 학생에게만 해당)  각 지점에 대해  연도별로 CWD, CDD 중 최대값, 두번째 최대, ... 5번째 최대까지를 구하라. 이들에 대해 중복된 시계열 그림을 다른 색깔과 다른 선 모양으로 그려라.
# 그림이 매우 많을 것이므로 년도별 변화가 뚜렷한 여섯 군데만 골라 제시하세요.

fn2 <- function(data){ # 5 번째 최대까지를 뽑아내기 위해서 임시로 만든 함수 
  temp <- unique(sort(data, decreasing = T))
  
  return(temp)
}

CWD_5 <-ddply(data_CWD, c("stn", "year"), summarise, a1 = fn2(CWD)[1], a2 = fn2(CWD)[2], a3 = fn2(CWD)[3],
              a4 = fn2(CWD)[4], a5 = fn2(CWD)[5])

CDD_5 <-ddply(data_CDD, c("stn", "year"), summarise, a1 = fn2(CDD)[1], a2 = fn2(CDD)[2], a3 = fn2(CDD)[3],
              a4 = fn2(CDD)[4], a5 = fn2(CDD)[5])

CWD_5 <- join(CWD_5, infor)
CDD_5 <- join(CDD_5, infor)

par(mfrow = c(2, 3))

draw_plot <- function(data, stn1, ylab){ # 그림을 그리는 함수, 범례표기는 그림의 크기가 작기에 생략 
  for (i in stn1){
    temp <- subset(data, stn == i)
    plot(temp[, 2], temp[, 3], main = temp$name[1], xlab = "year", ylab = ylab, type = "o", lty = 1, col = 1)
    # 검정, 실선
    lines(temp[, 2], temp[, 4], type = "o", lty = 2, col = 2) # 빨강, 대쉬선
    lines(temp[, 2], temp[, 5], type = "o", lty = 3, col = 3) # 초록, 점선
    lines(temp[, 2], temp[, 6], type = "o", lty = 4, col = 4) # 파랑, 점선과 대쉬선의 혼합
    lines(temp[, 2], temp[, 7], type = "o", lty = 5, col = 7) #  노랑, 긴 대쉬선
  }
}

draw_plot(CWD_5, u_stn, "CWD")
draw_plot(CDD_5, u_stn, "CDD")


# CWD : 강릉, 청주, 대구, 전주, 홍천, 합천
# CDD : 강릉, 울릉도, 성산, 해남, 밀양, 남해

x1 <- c("강릉", "청주", "대구", "전주", "홍천", "합천")
x2 <- c("강릉", "울릉도", "성산", "해남", "밀양", "남해")

x1_stn <- c()
x2_stn <- c()

for (i in x1){
  x1_stn <- c(x1_stn, infor$stn[infor$name == i])
}

for (i in x2){
  x2_stn <- c(x2_stn, infor$stn[infor$name == i])
}

par(mfrow = c(2, 3))
draw_plot(CWD_5, x1_stn, "CWD") # ans 
draw_plot(CDD_5, x2_stn, "CDD")


## 4 
# (통계 및 컴퓨터 전공 학생만 해당). 혹시 어떤 변화 경향이 있는지? 아울러 변화점을 찾는다면 어디가 되겠는지? 
# 통계적 추정이나 가설 검정을 이용하여 연구를 진행하라 (M-K test and CP finding).

# trend::mk.test
# trend::pettitt.test


data_y <- ddply(data, c("year"), summarise, prec = sum(prec)) # 연도별 총강수량(stn 무시)
# head(data_y)

# mk.test(x, alternative = c("two.sided", "greater", "less"), continuity = TRUE)
# 모든 경우에서 p_value 값이 높다. 
mk.test(data_y$prec, "two.sided") 
mk.test(data_y$prec, "greater") 
mk.test(data_y$prec, "less") 

# pettitt.test(x)
# p_value 값이 높다. 
p_test <-pettitt.test(data_y$prec)
p_test # 207 번째 데이터에서 절대값이 가장 큰 것(변화가 가장 큰 것)으로 관측되었다. 
p_t <- data_y$year[p_test$estimate] # 변화가 가장 큰 시점(2012)

# 엄청난 트렌드의 변화를 찾을 순 없지만 굳이 시험에서 찾은 지점을 표시하면 이와 같이 된다. 
g1 <- ggplot(data = data_y, aes(x = year, y = prec))
g1 + geom_line() + geom_vline(xintercept = p_t, linetype = 2, color = "red", size = 2)
