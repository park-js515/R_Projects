###########  전처리 
raw_data <- read.csv("raw_data.csv", header = T)
raw_data <- raw_data[, 1]
# head(raw_data)

temp_data <- raw_data

fn1 <- function(x){ # 1차 가공
  x1 <- x
  x1 <- gsub("[[:digit:]]", "", x1)
  x1 <- strsplit(x, " ")[[1]]

  for (i in 1:length(x1)) {
    if(x1[i] == "RT"){ #리트윗
      x1[i] <- ""
    }
    # @, #, http 로 시작하는 단어 제거 
    if(substr(x1[i], 1, 1) == "@" | substr(x1[i], 1, 1) == "#"| substr(x1[i], 1, 4) == "http") { 
      x1[i] <- ""
    }
    
    if(substr(x1[i], 1, 1) == "(" | substr(x1[i], 1, 1) == "["| substr(x1[i], 1, 1) == "<") { 
      x1[i] <- ""
    }
    
    if(substr(x1[i], 1, 2) == "|<" | substr(x1[i], 1, 1) == "/" | substr(x1[i], 1, 2) == "\"") { 
      x1[i] <- ""
    }
    
    if(substr(x1[i], 1, 2) == ".@") { 
      x1[i] <- ""
    } 
  }
  
  x1 <- paste(x1, collapse = " ")
  x1 <- trimws(x1)
  names(x1) = ""
  
  return(x1)
}

temp <- sapply(temp_data, FUN = fn1)
temp <- as.vector(temp) # names에 내용이 들어있는 것을 제거 

fn2 <- function(x){ # 2차 가공 
  x1 <- x
  x1 <- gsub(".*:", "", x1) # ex) NEWS: (내용) 에서 NEWS:를 제거
  x1 <- gsub("COVID-19|COVID|Covid-19|Covid|covid-19|covid", "", x1) # 검색에 쓴 단어를 제거 
  x1 <- gsub("\\+|\\,", "", x1)
  x1 <- gsub("\\d+", "", x1)
  x1 <- gsub("[[:cntrl:]]", "", x1) # Control character 제거
  return(x1)
}

temp2 <- sapply(temp, FUN = fn2)
temp2 <- as.vector(temp2)


myCorpus <- Corpus(VectorSource(temp2))
myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace)) # 공백 제거
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation)) # 구두점 제거
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers)) # 숫자 제거
myCorpus <- tm_map(myCorpus, content_transformer(tolower)) # 소문자로 변환
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), stopwords("english")) # 불용어 제거
myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace)) # 공백 제거
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation)) # 구두점 제거
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers)) # 숫자 제거
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), stopwords("english")) # 불용어 제거
# inspect(myCorpus)

