# 참고한 사이트
# https://r-pyomega.tistory.com/16
# https://hleecaster.com/twitter-api-developer/

########### 시작 
library(base64enc)
# library(KoNLP)
# library(RmecabKo)
library(rtweet)
library(igraph)
library(twitteR)
library(tm)
library(stringr)
library(dplyr)

# API_info : API_ID, API_Key, API_Key_Secret, Bearer_Token, Access_Token, Access_Token_Secret이 차례로 있음
API_info <- read.csv("API_info.csv", header = T)
dim(API_info)

# crawling setting
api_key <- API_info[1, 2]
api_secret_key <- API_info[1, 3]
access_token <- API_info[1, 5]
access_token_secret <- API_info[1, 6]
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

# getCurRateLimitInfo() # 어떠한 프레임으로 데이터를 크롤링하는지 알 수 있다? (잘 모르겠습니다.)

# 키워드 설정(다양하게 할 수 있겟지만 첫 번째 케이스만 하도록 하겠습니다.)
keyword_en1 <- enc2utf8("COVID-19")
# keyword_en2 <- enc2utf8("COVID19")
# keyword_en3 <- enc2utf8("COVID")
# keyword_en4 <- enc2utf8("covid-19")
# keyword_en5 <- enc2utf8("covid19")
# keyword_en6 <- enc2utf8("covid")

# COVID-19과 관련된 10000개의 영어 트윗을 12월 1일부터 12월 17일까지 크롤링
# 갯수가 부족하면 오류가 나오니 갯수와 시작과 끝을 적절하게 선택해야한다.
set.seed(1218)
data1 <- searchTwitter(keyword_en1, n = 10000, lang = "en", since = "2021-12-01", until = "2021-12-17")
data2 <- twListToDF(data1)
write.csv(data2, "raw_data.csv", row.names = F)
