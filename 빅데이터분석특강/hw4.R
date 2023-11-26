# 4차 과제 -------------------------------------------------------------------216803 박주성
# install.packages("ggmap")
library(plyr)
library(data.table)
library(zoo)
library(ggplot2)
library(ggmap)

## 0 데이터 준비 및 사용할 함수 정의
data <- read.csv("data_modify.csv", header = T)
infor <- read.csv("META_OBS_Infor.csv", header = T)

u_stn <- unique(data$stn)
u_year <- unique(data$year)
n_stn <- length(unique(data$stn))
n_year <- length(unique(data$year))


draw_plot <- function(data, u_stn){ # #1를 위한 함수 (검. 빨, 파, 노)
  for (i in u_stn){
    data_p <- data[data$stn == i, ]
    plot(data_p[, 2], data_p[, 3],type= "o", pch = 15, ylab = "prec(mm)", xlab = "year", main = data_p[1,7], las = 1, col = alpha(1, 0.5))
    lines(data_p[, 2], data_p[, 4],type= "o", pch = 15, las = 1, col = alpha(2, 0.8))
    lines(data_p[, 2], data_p[, 5],type= "o", pch = 15, las = 1, col = alpha(4, 0.8))
    lines(data_p[, 2], data_p[, 6],type= "o", pch = 15, las = 1, col = alpha(7, 0.5))
    
  }
}


##  1
# 각 지점에 대해  연도별로 AMP1 이 100, 200, 300, 400 이상인 날 수를 구하고, 이를 표와 그림으로 나타내라. 

data_AMP1_c <- ddply(data, c("stn", "year"), summarise, a100 = sum(prec >= 100), a200 = sum(prec >= 200), 
                       a300 = sum(prec >= 300), a400 = sum(prec >= 400))

data_AMP5_c <- ddply(data, c("stn", "year"), summarise, 
                   a300 = sum(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right") >= 300, na.rm = T),
                   a500 = sum(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right") >= 500, na.rm = T),
                   a700 = sum(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right") >= 700, na.rm = T),
                   a900 = sum(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right") >= 900, na.rm = T))

    
data_AMP1_c <- join(data_AMP1_c, infor)
data_AMP5_c <- join(data_AMP5_c, infor)
data_AMP1_c$year <-as.numeric(data_AMP1_c$year)
data_AMP5_c_year <- as.numeric(data_AMP5_c$year)

par(mfrow = c(3, 4))

draw_plot(data_AMP1_c, u_stn) # 전체 데이터 
draw_plot(data_AMP5_c, u_stn) # 전체 데이터 

# 춘천, 울진, 제주, 홍천, 남원, 산청 (변화가 나타나는 곳, AMP1, AMP5에 대해서 겹치는 것으로 선정함.)

x <- c("춘천", "울진", "제주", "홍천", "남원", "산청")
x_stn <- c()

for (i in x){
  x_stn <- c(x_stn, infor$stn[infor$name == i])
}

x_stn # 그림을 그릴 지역의 stn값 

par(mfrow = c(2, 3))
draw_plot(data_AMP1_c, x_stn) # 낮은 순서대로 (검, 빨, 파, 노)
draw_plot(data_AMP5_c, x_stn)


## 2
# 각 지점에 대해  연도별로 CWD, CDD 를 구하고, 이 통계량에 대해서도 제3차 과제의 #1 과 같은 표를 작성하여라.

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
  temp <- ddply(data, c("stn", "year"), summarise,min = min(CWD),  Q1 = quantile(CWD, 0.25), 
             Q2 = median(CWD), Q3 = quantile(CWD, 0.75), max = max(CWD), mean = mean(CWD))

  return(temp)
  
}

fn_CDD <- function(data){ # data summary
  temp <- ddply(data, c("stn", "year"), summarise,min = min(CDD),  Q1 = quantile(CDD, 0.25), 
                Q2 = median(CDD), Q3 = quantile(CDD, 0.75), max = max(CDD), mean = mean(CDD))
  
  return(temp)

}

data_CWD <- ddply(data, c("stn", "year"), summarise, CWD = CWD(prec))
data_CDD <- ddply(data, c("stn", "year"), summarise, CDD = CDD(prec))

CWD_s <- join(fn_CWD(data_CWD), infor)
CDD_s <- join(fn_CDD(data_CDD), infor)

head(CWD_s, 10)
head(CDD_s, 10)


## 3 

fn1 <- function(data, title){ # 그림을 그리기 위한 함수 
  map <- map_data("world2", region = "South Korea")
  
  ggplot(map) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "#87CEFA") +
    geom_point(data = data, aes(x = long, y = lat, size = data[, 1]), shape = 16, color = "red", alpha = 0.4) +
    scale_size_area(max_size = 10) +
    geom_text(data = data, aes(x = long + 0.2, y = lat + 0.2, label = name), size = 6) + 
    ggtitle(title) +
    theme(legend.title=element_blank())
}

idx_fn <- function(idx){ # 인덱스 갱신을 위한 함수 
  temp <- idx
  
  if (temp[2] < 3){
    temp[2] <- temp[2] + 1
  }
  else {
    temp[1] <- temp[1] + 1
    temp[2] <- 1
  }
  
  if(temp[1] == 6 & temp[2] == 1){
    cat("stop!, idx is max \n")
  }
  else {
    return(temp)
  }
}

draw_map <- function(data_list, idx){ # 인덱스 갱신으로 인한 데이터 갱신 및 플롯 생성 
  temp <- data_list[[idx[1]]][, c(idx[2] + 1, 5:7)]
  title <- paste(names(data_list)[idx[1]], "의 ", names(temp)[1], sep = "")
  
  fn1(temp, title)
}


# 데이터 요약 (시간적 통계량)
data_AMP1 <- ddply(data, c("stn", "year"), summarise, AMP1 = max(prec))
data_AMP5 <- ddply(data, c("stn", "year"), summarise, AMP5 = max(rollapply(prec, width = 5, FUN = sum, fill = NA, align = "right"), na.rm = T))
data_CWD_m <- CWD_s[, c(1, 2, 7)]
data_CDD_m <- CDD_s[, c(1, 2, 7)]
data_TP <- ddply(data, c("stn", "year"), summarize, TP = sum(prec))

AMP1_3 <- ddply(data_AMP1, c("stn"), summarise, Q2 = median(AMP1), 
                max = max(AMP1), p_95 = quantile(AMP1, 0.95)); AMP1_3 <- join(AMP1_3, infor)
AMP5_3 <- ddply(data_AMP5, c("stn"), summarise, Q2 = median(AMP5), 
                max = max(AMP5), p_95 = quantile(AMP5, 0.95)); AMP5_3 <- join(AMP5_3, infor)
CWD_m_3 <- ddply(data_CWD_m, c("stn"), summarise, Q2 = median(max),
                 max = max(max), p_95 = quantile(max, 0.95)); CWD_m_3 <- join(CWD_m_3, infor)
CDD_m_3 <- ddply(data_CDD_m, c("stn"), summarise, Q2 = median(max),
                 max = max(max), p_95 = quantile(max, 0.95)); CDD_m_3 <- join(CDD_m_3, infor)
TP_3 <- ddply(data_TP, c("stn"), summarise, Q2 = median(TP),
              max = max(TP), p_95 = quantile(TP, 0.95)); TP_3 <- join(TP_3, infor)

data_list <- list(AMP1 = AMP1_3, AMP5 = AMP5_3, CWD_max = CWD_m_3, CDD_max = CDD_m_3, TP = TP_3)

idx <- c(1, 1)
idx
draw_map(data_list, idx)

##################### 이부분의 코드를 오류가 있을 때까지 반복하면 된다. 
idx <- idx_fn(idx)
idx
draw_map(data_list, idx)
##################### 반복문을 통해서 ggplot이 되지 않아서 이렇게 실행했습니다. 


## 4

draw_plot2 <- function(data){ # #1를 위한 함수 (검. 빨, 파, 노)
  for (i in u_stn){
    data_p <- data[data$stn == i, ]
    plot(data_p[, 2], data_p[, 3],type= "o", pch = 15, ylab = "prec(mm)", xlab = "year", main = data_p[1,7], las = 1, col = alpha(1, 0.5))
    lines(data_p[, 2], data_p[, 4],type= "o", las = 1, col = alpha(2, 0.8))
    lines(data_p[, 2], data_p[, 5],type= "o", las = 1, col = alpha(4, 0.8))
    lines(data_p[, 2], data_p[, 6],type= "o", las = 1, col = alpha(7, 0.5))
    
  }
}
# 데이터 요약 (공간적 통계량)
AMP1_ss <- ddply(data_AMP1, c("year"), summarise, Q2 = median(AMP1), 
                max = max(AMP1), p_95 = quantile(AMP1, 0.95)); 
AMP5_ss <- ddply(data_AMP5, c("year"), summarise, Q2 = median(AMP5), 
                max = max(AMP5), p_95 = quantile(AMP5, 0.95)); 
CWD_m_ss <- ddply(data_CWD_m, c("year"), summarise, Q2 = median(max),
                 max = max(max), p_95 = quantile(max, 0.95)); 
CDD_m_ss <- ddply(data_CDD_m, c("year"), summarise, Q2 = median(max),
                 max = max(max), p_95 = quantile(max, 0.95)); 
TP_ss <- ddply(data_TP, c("year"), summarise, Q2 = median(TP),
              max = max(TP), p_95 = quantile(TP, 0.95));

data_list2 <- list(AMP1 = AMP1_ss, AMP5 = AMP5_ss, CWD_max = CWD_m_ss, CDD_max = CDD_m_ss, TP = TP_ss)


draw_plot2 <- function(data){ # #4를 위한 함수 (검. 빨, 파) -> (max, Q2, p_95)
  for (i in 1:5){
    data_p <- data[[i]]
    ylab <- names(data)[i]
    title <- paste(names(data)[i])
    
    par(mfrow = c(1, 1))
    plot(data_p[, 1], data_p[, 3],type= "o", pch = 15, ylab = ylab, xlab = "year",
         las = 1, col = alpha(1, 0.5), ylim = c(0, max(data_p[, 3])))
    lines(data_p[, 1], data_p[, 2],type= "o", pch = 15, las = 1, col = alpha(2, 0.8))
    lines(data_p[, 1], data_p[, 4],type= "o", pch = 15, las = 1, col = alpha(4, 0.8))
    title(title)
    legend("bottomleft", c("max", "Q2", "p_95"), col = c(1, 2, 4), pch = c(15, 15, 15), cex = .8, horiz = T)
  }
}

draw_plot2(data_list2) # CWD, CDD는 max와 p_95가 겹치기에 검은색 선이 나오지 않는다. 
