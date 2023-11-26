############ 10000개 전체를 쓰는데는 조금 무리가 있는 것으로 판단이 됩니다. 
set.seed(1219)
case <- sample(10000, 2000) # 10000개 중 2000개를 랜덤으로 사용합니다. 
TDM <- TermDocumentMatrix(myCorpus[case], control = list(wordLengths = c(2, Inf)))
mydata.dtm <- removeSparseTerms(TDM, sparse = 0.95)
mydata.df <- as.matrix(TDM)
mydata.df[mydata.df >= 1] <- 1
mydata.df2 <- mydata.df %*% t(mydata.df)

write.csv(mydata.df2, "mydata1.csv", row.names = F)


covid <- graph.adjacency(mydata.df2, weighted = T, mode = "undirected")

g <- simplify(covid)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$label.cex <- 1.5*(V(g)$degree/max(V(g)$degree))
V(g)$size <- 15*(V(g)$degree/max(V(g)$degree))
E(g)$width <- 4*(E(g)$weight/max(E(g)$weight))
condition <- V(g)[degree(g) < 10]
g1 <- delete.vertices(g, condition)

# head(sort(degree(g1)))
# table(degree(g1))
set.seed(1219)
x11()
plot(g1)

# 아마도 2000개의 트윗을 사용하다보니 좋은 것 같지는 않습니다. 
# 히스토그램이나 테이블을 활용해서 적절한 지점 밑을 제거하고 시각화하는 것이 바람직할 것으로 보입니다.
# table(degree(g1))
hist(degree(g1), nclass = 20)
abline(v = 200, col = "red", lwd = 3)
text(825, 1000, "degree(g1) = 200", cex = 1)
# degree(g1) > 200 이상인 곳에서 그림을 그리면 이상적일 것 같습니다. 

condition2 <- V(g)[degree(g) < 200]
g2 <- delete.vertices(g, condition2)

set.seed(1219)
x11()
plot(g2)

# 전처리 과정에서 ..., 's 가 제거되지 않았고, 최대빈도를 가지므로 이를 반영하여 다시 그림을 그립니다. 

g <- simplify(covid) # 처음의 상황으로 돌림
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# tail(sort(degree(g)))

condition3 <- V(g)[degree(g) > 1100 | degree(g) < 200 | degree(g) == 229] 
g1 <- delete.vertices(g, condition3)
V(g1)$label.cex <- 1.5*(V(g1)$degree/max(V(g1)$degree))
V(g1)$size <- 15*(V(g1)$degree/max(V(g1)$degree))
E(g1)$width <- 4*(E(g1)$weight/max(E(g1)$weight))



set.seed(1219)
x11()
plot(g1)


# 전처리가 완벽하지는 않지만 대략적인 것을 파악할 수 있을 것으로 보입니다.
# 일단 백신과 관련된 내용이 가장 많이 나와서 vaccine, vaccinated, vaccines, boost(부스터샷)가 있습니다.
# 것으로 확인되며, 12월 1일 ~ 17일 시점에 오미크론에 대한 관심이 있는 것으로 확인할 수 있습니다. 


############## 3
library(proxy)
# mydata.df[1:5, 1:5]

# 덴드로이드그램으로 하기엔 숫자가 많아 보입니다. 
d <- dist(t(mydata.df), method = "cosine")
hc <- hclust(d, method = "ward.D")
x11()
plot(hc)

# k_means 클러스터링 
set.seed(1219)
k_means <- kmeans(d, 5)
c_num <- k_means$cluster # 클러스터 번호 저장
# k_means$size # 60  264 1400  106  170

# 클러스터 지정 
c1 <- rownames(mydata.df)[c_num == 1]
c2 <- rownames(mydata.df)[c_num == 2]
c3 <- rownames(mydata.df)[c_num == 3]
c4 <- rownames(mydata.df)[c_num == 4]
c5 <- rownames(mydata.df)[c_num == 5]

clist <- list(c1, c2, c3, c4, c5) # 하나의 리스트로 묶어서 반복문 수행에 용이하게 만듦
# 주요 키워드
keyword <- names(degree(g3)) # 최종 그림의 vertex들

# cluster vs keyword matrix 클러스터에 해당 키워드가 있다면 1을 아니면, 0으로 나타내보자 
ckmatrix <- matrix(0, 5, length(keyword))
colnames(ckmatrix) <- keyword
# ckmatrix


for (i in 1:5){
  for (j in 1:length(keyword)){
    if (sum(clist[[i]] == keyword[j]) > 0){
      ckmatrix[i, j] <- 1
    }
  }
}

ckmatrix


# k_means 클러스터링 ver.2
set.seed(1220)
k_means <- kmeans(d, 5)
c_num <- k_means$cluster # 클러스터 번호 저장
# k_means$size # 60  264 1400  106  170

# 클러스터 지정 
c1 <- rownames(mydata.df)[c_num == 1]
c2 <- rownames(mydata.df)[c_num == 2]
c3 <- rownames(mydata.df)[c_num == 3]
c4 <- rownames(mydata.df)[c_num == 4]
c5 <- rownames(mydata.df)[c_num == 5]

clist <- list(c1, c2, c3, c4, c5) # 하나의 리스트로 묶어서 반복문 수행에 용이하게 만듦
# 주요 키워드
keyword <- names(degree(g3)) # 최종 그림의 vertex들

# cluster vs keyword matrix 클러스터에 해당 키워드가 있다면 1을 아니면, 0으로 나타내보자 
ckmatrix <- matrix(0, 5, length(keyword))
colnames(ckmatrix) <- keyword
# ckmatrix


for (i in 1:5){
  for (j in 1:length(keyword)){
    if (sum(clist[[i]] == keyword[j]) > 0){
      ckmatrix[i, j] <- 1
    }
  }
}

ckmatrix

# k_means 클러스터링 ver.3
set.seed(1220)
k_means <- kmeans(d, 8)
c_num <- k_means$cluster # 클러스터 번호 저장
# k_means$size # 60  264 1400  106  170

# 클러스터 지정 
c1 <- rownames(mydata.df)[c_num == 1]
c2 <- rownames(mydata.df)[c_num == 2]
c3 <- rownames(mydata.df)[c_num == 3]
c4 <- rownames(mydata.df)[c_num == 4]
c5 <- rownames(mydata.df)[c_num == 5]
c6 <- rownames(mydata.df)[c_num == 6]
c7 <- rownames(mydata.df)[c_num == 7]
c8 <- rownames(mydata.df)[c_num == 8]

clist <- list(c1, c2, c3, c4, c5, c6, c7, c8) # 하나의 리스트로 묶어서 반복문 수행에 용이하게 만듦
# 주요 키워드
keyword <- names(degree(g3)) # 최종 그림의 vertex들

# cluster vs keyword matrix 클러스터에 해당 키워드가 있다면 1을 아니면, 0으로 나타내보자 
ckmatrix <- matrix(0, 8, length(keyword))
colnames(ckmatrix) <- keyword
# ckmatrix


for (i in 1:8){
  for (j in 1:length(keyword)){
    if (sum(clist[[i]] == keyword[j]) > 0){
      ckmatrix[i, j] <- 1
    }
  }
}

ckmatrix
