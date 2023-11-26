# ,로 구분된 문자열에서 두 개의 숫자를 추출
extract1 <- function(text){
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  
  return(as.numeric(split))
}



# ,로 구분된 문자열에서 두 개의 숫자를 추출 2
extract2 <- function(text) {
  text <- gsub(" ", "", text)
  split1 <- strsplit(text, ",", fixed = FALSE)[[1]]
  split2 <- c()
  len <- length(split1)
  
  for (i in 1:len){
    if (grepl(":", split1[i]) == T){
      temp <- strsplit(split1[i], ":", fixed = F)[[1]]
      split2 <- c(split2, as.numeric(temp[1]):as.numeric(temp[2]))
    }
    else {
      split2 <- c(split2, split1[i])
    }
  }
  
  return(sort(unique(as.numeric(split2))))
}

# row data 입력 시 t와 se 를 반환
ftn1 <- function(data, g1Index, g2Index, type = T){
  # data : row(variable), col(observation : g1 vs g2)
  # g1Index : group1 index
  # g2Index : group2 index, if -1 then data corresponding to the remainder of g1Index
  # type : if T(TRUE) then use t with pooled variance else if F(FALSE) then use t with Satterthwaite degree of freedom, default is T
  
  df <- c() # return of this function
  len <- dim(data)[1]
  
  d1 <- data.frame(data[, g1Index])
  
  if (g2Index == -1){
    d2 <- data.frame(data[, -g1Index])
  }
  else {
    d2 <- data.frame(data[, g2Index])
  }
  
  for (i in 1:len){
    act <- t.test(d1[i, ], d2[i, ], var.equal = type)
    temp <- c(act$statistic, act$stderr)
    
    df <- rbind(df, temp)
  }
  
  df <- data.frame(df)
  colnames(df) <- c("t", "se")
  rownames(df) <- rownames(data)
  
  return(df)
}

# t 입력 시 p-value로 변환
ftn2 <- function(t, len1, len2){
  tempt <- t[, 1]
  temp <- sapply(tempt, 
                 FUN = function(t){
                   min(pt(t, df = (len1 + len2 - 2), lower.tail = F) * 2, pt(t, df = (len1 + len2 - 2), lower.tail = T) * 2) 
                 })
  return(temp)
}

# z 입력 시 p-value로 변환 
ftn3 <- function(z){
  tempz <- z[, 1]
  temp <- sapply(tempz,
                 FUN = function(z){
                   min(pnorm(z, lower.tail = F) * 2, pnorm(z, lower.tail = T) * 2)
                 })
  return(temp)
}

# t -> z
ftn4 <- function(t, len1, len2){
  tempt <- t[, 1]
  temp1 <- sapply(tempt,
                  FUN = function(t){
                    pt(t, df = (len1 + len2 - 2))
                  })
  temp2 <- sapply(temp1,
                  FUN = function(q){
                    qnorm(q)
                  })
  
  return(temp2)
}

# z -> t
ftn5 <- function(z, len1, len2){
  tempz <- z[, 1]
  temp1 <- sapply(tempz,
                  FUN = function(z){
                    pnorm(z)
                  })
  temp2 <- sapply(temp1,
                  FUN = function(q){
                    qt(q, df = (len1 + len2 - 2))
                  })
  
  return(temp2)
}


# only for 1D, use normal dist.
draw_true <- function(range, pr1, m1, s1, pr2, m2, s2, plot = 1){
  
  n <- length(c(pr1, pr2))
  null <- length(pr1)
  p <- c(abs(pr1), abs(pr2))
  m <- c(m1, m2)
  s <- c(s1, s2)
  
  ranges <- seq(range[1], range[2], length.out = 1000)
  mat <- matrix(0, nrow = 1000, ncol = n + 4)
  colnames(mat)[(n + 1):(n + 4)] <- c("f", "f0", "f1", "fdr")
  
  if (sum(p) != 1){
    p <- p/sum(p)
  }
  
  np <- rep(0, n)
  np[1:null] <- p[1:null]
  np <- np/sum(np)
  p0 <- sum(p[1:null])
  
  ap <- rep(0, n)
  ap[(null + 1):n] <- p[(null + 1):n]
  ap <- ap/sum(ap)
  p1 <- sum(p[(null + 1):n])
  
  if (length(m) < n){
    temp <- n - length(m)
    m <- c(m, rep(m[length(m)], temp))
  }
  
  if (length(s) < n){
    temp <- n - length(s)
    s <- c(s, rep(s[length(s)], temp))
  }
  
  for (i in 1:n){
    mat[, i] <- dnorm(ranges, mean = m[i], sd = s[i])
  }
  
  mat[, n + 1] <- apply(mat, 1, function(mat){
    len <- n
    temp <- 0
    
    for (i in 1:len){
      temp <- temp + mat[i] * p[i]
    }
    
    return(temp)
  })
  
  mat[, n + 2] <- apply(mat, 1, function(mat){
    len <- 1:null
    temp <- 0
    
    for (i in len){
      temp <- temp + mat[i] * np[i]
    }
    
    return(temp)
  })
  
  mat[, n + 3] <- apply(mat, 1, function(mat){
    len <- (null + 1):n
    temp <- 0
    
    for (i in len){
      temp <- temp + mat[i] * ap[i]
    }
    
    return(temp)
  })
  
  mat[, n + 4] <- apply(mat, 1, function(mat){
    
    return(min(p0 * mat[n + 2] / mat[n + 1], 1))
  })
  
  # f
  if (plot == 1){
    lines(ranges, mat[, n + 1], type = "l")
  }
  
  # p0*f0
  if (plot == 2){
    lines(ranges, p0 * mat[, n + 2], type = "l")
  }
  
  # p1*f1
  if (plot == 3){
    lines(ranges, p1 * mat[, n + 3], type = "l")
  }
  
  # fdr
  if (plot == 4){
    lines(ranges, mat[, n + 4], type = "l")
  }
  
  abline(v = m, lty = 2)
  # return(mat)
}

# # EX
# range <- c(-4, 4)
# ranges <- seq(-4, 4, length.out = 1000)
# pr1 <- 0.8
# m1 <- 0
# s1 <- 1
# 
# pr2 <- c(0.1, 0.1)
# m2 <- c(-2, 2)
# s2 <- c(1, 1)
# 
# plot(0, type = "n", xlim = c(-4, 4), ylim = c(0, 2))
# a <- draw_true(range, pr1, m1, s1, pr2, m2, s2)


# vector with NA (for making output csv)
vwn <- function(n_na, data){
  
  if (is.na(data[1])){
    d <- NA
  }
  else {
    d <- sort(data)
  }
  
  p <- length(d)
  
  if (n_na - p > 0){
    z <- c(d, rep(NA, n_na - p))
  }
  else {
    z <- d
  }
  
  return(z)
}

vwn2 <- function(n_na, data){
  d <- sort(data)
  p <- length(d)
  
  if (n_na - p > 0){
    z <- c(d, rep(NA, n_na - p))
  }
  else {
    z <- d
  }
  
  return(z)
}