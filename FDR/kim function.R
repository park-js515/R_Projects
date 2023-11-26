# 1 tor, inter plot
# 2 tor, inter lines
# 3 tor, union plot
# 4 tor, union lines
# 5 vol, inter plot
# 6 vol, inter lines
# 7 vol, union plot
# 8 vol, union lines
library(locfdr)
library(fdrtool)
require(mclust)

yrkim <- function(data, idx1, idx2, cutoff = c(0.05, 0.1, 0.15), trc = 0.4, seed_num = 0){

  ## 1. parameter settings
  p <- ncol(data)
  q <- length(cutoff)
  len1 <- length(idx1)
  len2 <- length(idx2)

  if (idx2[1] == -1){
    len2 <- p - len1
  }

  len <- len1 + len2
  col <- c(3, 4, 2)
  ls.trc <- 0.5 * trc

  result.list <- list()
  result.i <- matrix(0, q, 6)
  result.i <- data.frame(result.i)
  colnames(result.i) <- c("cutoff", "cut.t", "cut.ls", "H_01 rej", "H_01 & H_02 rej", "H_02 rej")
  result.u <- matrix(0, q, 6)
  result.u <- data.frame(result.u)
  colnames(result.u) <- c("cutoff", "cut.t", "cut.ls", "H_01 rej", "H_01 & H_02 rej", "H_02 rej")

  if (idx2[1] != -1){
    nd <- data[, c(idx1, idx2)]
  }
  else {
    nd <- data
  }

  # normalization 
  
  # nd <- log(1 + nd)
  # 
  # for(i in (1:len)){
  #   nd[, i] <- nd[, i]/sum(nd[, i])
  # }

  X <- nd
  grp <- rep(c("g1", "g2"), c(len1, len2))
  g1 <- len1
  g2 <- len2
  index <- 1:g1
  pop1 <- X[, index]
  pop2 <- X[, -index]
  mm <- cbind(pop1, pop2)
  n <- nrow(mm)

  ## 2. calculate t-stat, p-value
  df <- matrix(0, n, 3) # making the null "index", t-value", "p-value"
  colnames(df) <- c("index", "t-value", "p_value")

  for(i in 1:n){
    df[i,1:3] <-c (i, #index
                 t.test(mm[i, (1:g1)], mm[i, (g1 + 1):(g1 + g2)], var.equal = TRUE)$statistic, #t-statistics
                 t.test(mm[i, (1:g1)], mm[i, (g1 + 1):(g1 + g2)], var.equal = TRUE)$p.value) # p-value
  }
  Tval <- df[, 2]
  pvalue<-df[, 3]
  rejection_table <- matrix(0, nrow = length(cutoff), ncol = 2)
  colnames(rejection_table) <- c("Kim(i)", "Kim(u)")
  rownames(rejection_table) <- cutoff


  ## 3.1 Efron procedure
  Mean_g1 <- apply(mm[, grp == "g1"], 1, mean);
  Mean_g2 <- apply(mm[, grp == "g2"], 1, mean)
  S1 <- apply(mm[, 1:g1], 1, var);
  S2 <- apply(mm[, (g1 + 1):(g1 + g2)], 1, var)
  Sp <- sqrt(((g1 - 1) * S1 + (g2 - 1) * S2) * (1/g1 + 1/g2)/(g1 + g2 - 2))
  a0 <- sort(Sp)[length(Sp) * 0.9];
  meandiff <- (Mean_g1 - Mean_g2)

  Tval <- meandiff/(Sp) ## two sample T-statistics
  Zval <- qnorm(pt(Tval, df = g1 + g2 - 2))
  Zval[Zval == Inf] <- max(Zval[!is.infinite(Zval)])
  Zval[Zval == -Inf] <- min(Zval[!is.infinite(Zval)])


  ## 3.2 Youngrae kim procedure
  r <- seq(0.01, 1, by = 0.0001)
  res_r <- rep(0, length(r))
  ar <- rep(0, length(r))
  br <- rep(0, length(r))
  cr <- rep(0, length(r))

  # t based fdr
  fdr.t <- fdrtool(pvalue, statistic = "pvalue", plot = F)

  ls <- log(sqrt(((g1 - 1) * S1 + (g2 - 1) * S2) * (1/g1 + 1/g2)/(g1 + g2 - 2))) # for equality tstat
  set.seed(seed_num)
  res <- Mclust(ls, modelNames = c("V")) # variable/unqual variance (one-dimensional)
  par <- c(res$parameters$mean[1], res$parameter$variance$sigmasq[1]) # f_0 Parameter
  pi0 <- res$parameters$pro[1] # Set pi_0 = 1
  set.seed(seed_num)
  f <- approxfun(density(ls)) ## Approximation of f

  # log(se) based fdr
  fdr.ls <- sapply(1:length(ls),
                   FUN = function(j)(pi0 * dnorm(ls[j], mean = par[1], sd = sqrt(par[2]))/f(ls[j])))

  # determine cutoff-point for union
  for(j in 1:length(r)){
    h1.index <- fdr.t$lfdr < r[j] ## H_01 Reject
    cut.ls <- min(ls[((fdr.ls < r[j]) & (ls >= quantile(ls, ls.trc)))]) # Please check the value 0.2(more or less ?)
    ##cut.ls<-min(ls[fdr.ls<r[j]]) ## Cut point;
    h2.index <- (ls > cut.ls)  ## The fact that fdr.ls<a2 implies ls is Big. ; H_02 Reject

    ar[j] <- sum(h1.index * h2.index)           ##  a(r)
    br[j] <- sum(h1.index * (1 - h2.index))     ##  b(r)
    cr[j] <- sum((1 - h1.index) * h2.index)     ##  c(r)
    res_r[j] <- r[j] * (1 + min(br[j]/ar[j], cr[j]/ar[j]))
  }

  # two-stage procedure union and intersection
  for(x in 1:length(cutoff)){
    ## 3.2.1 kim's union
    cat("Kim's Method :", "qvalue is", cutoff[x],"\n")
    qvalue <- cutoff[x]
    rstar <- min(r[res_r > qvalue], na.rm = TRUE)
    h1.index <- fdr.t$lfdr < rstar ## H_01 Reject
    cut.ls <- min(ls[((fdr.ls < rstar) & (ls >= quantile(ls, qvalue)))])

    h2.index <- (ls > cut.ls)
    ind.our <- h1.index * h2.index; # union
    a5 <- which(ind.our == 1)
    la5 <- length(a5)
    rejection_table[x, 2] <- c(la5)
    result.list[[q + x]] <- a5

    # sum((fdr.t$lfdr < rstar)*(fdr.ls < rstar))

    ## 3.2.2. kim's intersection
    rej.index.ours <- (fdr.t$lfdr < qvalue/2)|((fdr.ls < (qvalue/2)) & (ls >= quantile(ls, qvalue))) # intersection
    la6 <- sum(rej.index.ours)
    a6 <- which(rej.index.ours == 1)
    result.list[[x]] <- a6

    rejection_table[x, 1] <- c(la6)

    # sum((fdr.t$lfdr < a/2)|(fdr.ls < a/2))
  }

  names(result.list) <- c(paste0("rej_i_", cutoff), paste0("rej_u_", cutoff))
  result.list$rejection_table <- rejection_table

  for(i in length(cutoff):1){
    a = cutoff[i]
    cut.t <- min(abs(Tval[fdr.t$lfdr < a/2]))
    cut.ls <- min(ls[((fdr.ls < a/2) & (ls >= quantile(ls, a)))])##
    #cut.ls<-min(ls[fdr.ls<a/2]) ##
    c <- exp(cut.ls + log(cut.t))

    result.i[i, ] <- c(a, cut.t, cut.ls,
                       sum(fdr.t$lfdr<a/2), sum((fdr.t$lfdr<a/2)|(fdr.ls<a/2)), sum(fdr.ls<a/2))
    }

  for(i in length(cutoff):1){
    a = cutoff[i]
    rstar <- min(r[res_r > a], na.rm = T)
    cut.t <- max(abs(Tval[fdr.t$lfdr > rstar]))
    cut.ls <- min(ls[((fdr.ls < rstar) & (ls >= quantile(ls, a)))])##
    #cut.ls<-min(ls[fdr.ls<rstar]) ##
    c <- exp(cut.ls+log(cut.t))

    result.u[i, ] <- c(a, cut.t, cut.ls,
                       sum(fdr.t$lfdr<rstar), sum((fdr.t$lfdr<rstar)*(fdr.ls<rstar)), sum(ls>cut.ls))
    }


  #! please check
  # result.u[, 5] <- rejection_table[, 2]

  result.list$result.i <- result.i
  result.list$result.u <- result.u

  result.list2 <- list(meandiff = meandiff, ls = ls, fdr.ls = fdr.ls, col = col,
                       lfdr = fdr.t$lfdr, Tval = Tval, r = r, res_r = res_r,
                       pvalue = pvalue, ls.trc = ls.trc, seed_num = seed_num,
                       cutoff = cutoff, g1 = g1, g2 = g2)
  
  
  result <- list(result.list, result.list2)
  return(result)
  }


kim_plot <- function(result, plot = F, main = "", ylim = NULL, xlim = NULL){
  result2 <- result[[2]]
  meandiff <- result2$meandiff
  ls <- result2$ls
  fdr.ls <- result2$fdr.ls
  col <- result2$col
  fdr.t <- list(lfdr = result2$lfdr)
  Tval <- result2$Tval
  r <- result2$r
  res_r <- result2$res_r
  pvalue <- result2$pvalue
  ls.trc <- result2$ls.trc
  seed_num <- result2$seed_num
  cutoff <- result2$cutoff
  g1 <- result2$g1
  g2 <- result2$g2
  
  
  # 1 tor, inter plot
  if (plot == 1){
    plot(meandiff, ls, pch = ".", xlim = xlim, ylim = ylim, main = main,
         xlab = "Mean Difference", ylab = "log(Standard Error)")
    
    for(i in length(cutoff):1){
      a = cutoff[i]
      cut.t <- min(abs(Tval[fdr.t$lfdr < a/2]))
      cut.ls <- min(ls[((fdr.ls < a/2) & (ls >= quantile(ls, a)))])##
      #cut.ls<-min(ls[fdr.ls<a/2]) ##
      c <- exp(cut.ls + log(cut.t))
      
      
      segments(x0 = -c, x1 = c,y0 = cut.ls, col = col[i], lty = 2, lwd = 2)
      eq = function(xx){-log(cut.t) + log(xx)}
      eql = function(xx){-log(cut.t) + log(-xx)}
      curve(eq, to = c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eql, from = -c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      
      point_col_md <- meandiff[fdr.t$lfdr < a/2  | (ls >= cut.ls)]
      point_col_ls <- ls[fdr.t$lfdr < a/2 | (ls >= cut.ls)]
      points(point_col_md, point_col_ls, col = col[i], cex = 0.5, pch = 16)
    }
  }
  
  # 2 tor, inter lines
  if (plot == 2){
    for(i in length(cutoff):1){
      a = cutoff[i]
      cut.t <- min(abs(Tval[fdr.t$lfdr < a/2]))
      cut.ls <- min(ls[((fdr.ls < a/2) & (ls >= quantile(ls, a)))])##
      #cut.ls<-min(ls[fdr.ls<a/2]) ##
      c <- exp(cut.ls + log(cut.t))
      
      
      segments(x0 = -c, x1 = c,y0 = cut.ls, col = col[i], lty = 2, lwd = 2)
      eq = function(xx){-log(cut.t) + log(xx)}
      eql = function(xx){-log(cut.t) + log(-xx)}
      curve(eq, to = c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eql, from = -c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      
      point_col_md <- meandiff[fdr.t$lfdr < a/2  | (ls >= cut.ls)]
      point_col_ls <- ls[fdr.t$lfdr < a/2 | (ls >= cut.ls)]
      points(point_col_md, point_col_ls, col = col[i], cex = 0.5, pch = 16)
    }
  }
  
  # 3 tor, union plot
  if (plot == 3){
    plot(meandiff, ls, pch = ".", xlim = xlim, ylim = ylim, main = main,
         xlab = "Mean Difference", ylab = "log(Standard Error)")
    
    for(i in length(cutoff):1){
      a = cutoff[i]
      rstar <- min(r[res_r > a], na.rm = T)
      cut.t <- max(abs(Tval[fdr.t$lfdr > rstar]))
      cut.ls <- min(ls[((fdr.ls < rstar) & (ls >= quantile(ls, a)))])##
      #cut.ls<-min(ls[fdr.ls<rstar]) ##
      c <- exp(cut.ls+log(cut.t))
      
      
      segments(x0 = c, x1 = 100, y0 = cut.ls, col = col[i], lty = 2,lwd = 2)
      segments(x0 = -100, x1 = -c, y0 = cut.ls, col = col[i], lty = 2, lwd = 2)
      eq = function(xx){-log(cut.t) + log(xx)} # log mean diff
      eql = function(xx){-log(cut.t) + log(-xx)} #
      curve(eq, from = c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eql, to = -c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      
      point_col_md <- meandiff[fdr.t$lfdr < rstar & fdr.ls < rstar & (ls >= cut.ls)]
      point_col_ls <- ls[fdr.t$lfdr < rstar & (fdr.ls < rstar) & (ls >= cut.ls)]
      points(point_col_md, point_col_ls, col = col[i], cex = 0.5, pch = 16)
    }
  }
  
  # 4 tor, union lines
  if (plot == 4){
    for(i in length(cutoff):1){
      a = cutoff[i]
      rstar <- min(r[res_r > a], na.rm = T)
      cut.t <- max(abs(Tval[fdr.t$lfdr > rstar]))
      cut.ls <- min(ls[((fdr.ls < rstar) & (ls >= quantile(ls, a)))])##
      #cut.ls<-min(ls[fdr.ls<rstar]) ##
      c <- exp(cut.ls+log(cut.t))
      
      
      segments(x0 = c, x1 = 100, y0 = cut.ls, col = col[i], lty = 2,lwd = 2)
      segments(x0 = -100, x1 = -c, y0 = cut.ls, col = col[i], lty = 2, lwd = 2)
      eq = function(xx){-log(cut.t) + log(xx)} # log mean diff
      eql = function(xx){-log(cut.t) + log(-xx)} #
      curve(eq, from = c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eql, to = -c, add = TRUE, col = col[i], lty = 2, lwd = 2)
      
      point_col_md <- meandiff[fdr.t$lfdr < rstar & fdr.ls < rstar & (ls >= cut.ls)]
      point_col_ls <- ls[fdr.t$lfdr < rstar & (fdr.ls < rstar) & (ls >= cut.ls)]
      points(point_col_md, point_col_ls, col = col[i], cex = 0.5, pch = 16)
    }
  }
  
  # 5 vol, inter plot
  if (plot == 5){
    plot(meandiff, -log(pvalue, 10), pch = ".", xlim = xlim, ylim = ylim, main = main,
         xlab = "Mean Difference", ylab = "-log10(p-value)")
    
    for(i in length(cutoff):1){
      a = cutoff[i]
      ## point
      cut.pval <- max(-log(pvalue[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)], 10))
      md.out <- meandiff[fdr.t$lfdr < (a/2) | fdr.ls < (a/2)] # outside
      md.in <- meandiff[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)] # inside
      md.in.pos <- md.in[md.in > 0];
      md.in.neg <- md.in[md.in <= 0] # inside md max, min
      
      ls.in <- ls[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2) ]
      cut.ls <- max(ls.in) ## Cut point;
      
      
      Tval.in <- Tval[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2) ]
      cut.Tval <- max(Tval.in) ## Cut point;
      
      
      pval.a <- pvalue[fdr.t$lfdr < a/2 | fdr.ls < a/2]
      mlp.a <- (-log(pval.a, 10))
      
      points(md.out, mlp.a, col = col[i], cex = 0.5, pch = 16)
      
      ## line
      cut.pval <- max(-log(pvalue[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)], 10))
      #cut.pval.pos<-(-log(pvalue[meandiff==max(md.n)],10))
      #cut.pval.neg<-(-log(pvalue[meandiff==min(md.n)],10))
      
      eq = function(x){-log(2 - 2 * pt(abs(x)/exp(cut.ls), df = g1 + g2 - 2), 10)}
      grid1 <- seq(min(md.in.neg) * (3/2), 0, length.out = 10000)
      grid2 <- seq(0, max(md.in.pos) * (3/2), length.out = 10000)
      
      
      segments(x0 = min(grid2[eq(grid2) > cut.pval]), x1 = max(grid1[eq(grid1) > cut.pval]),
               y0 = cut.pval,col = col[i], lty = 2, lwd = 2)
      curve(eq, from = min(md.in.pos), to = min(grid2[eq(grid2) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eq, from = max(grid1[eq(grid1) > cut.pval]), to = max(md.in.neg), add = TRUE, col = col[i], lty = 2, lwd = 2)
    }
  }
  
  # 6 vol, inter lines
  if (plot == 6){
    for(i in length(cutoff):1){
      a = cutoff[i]
      ## point
      cut.pval <- max(-log(pvalue[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)], 10))
      md.out <- meandiff[fdr.t$lfdr < (a/2) | fdr.ls < (a/2)] # outside
      md.in <- meandiff[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)] # inside
      md.in.pos <- md.in[md.in > 0];
      md.in.neg <- md.in[md.in <= 0] # inside md max, min
      
      ls.in <- ls[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2) ]
      cut.ls <- max(ls.in) ## Cut point;
      
      
      Tval.in <- Tval[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2) ]
      cut.Tval <- max(Tval.in) ## Cut point;
      
      
      pval.a <- pvalue[fdr.t$lfdr < a/2 | fdr.ls < a/2]
      mlp.a <- (-log(pval.a, 10))
      
      points(md.out, mlp.a, col = col[i], cex = 0.5, pch = 16)
      
      ## line
      cut.pval <- max(-log(pvalue[!fdr.t$lfdr < (a/2) & !fdr.ls < (a/2)], 10))
      #cut.pval.pos<-(-log(pvalue[meandiff==max(md.n)],10))
      #cut.pval.neg<-(-log(pvalue[meandiff==min(md.n)],10))
      
      eq = function(x){-log(2 - 2 * pt(abs(x)/exp(cut.ls), df = g1 + g2 - 2), 10)}
      grid1 <- seq(min(md.in.neg) * (3/2), 0, length.out = 10000)
      grid2 <- seq(0, max(md.in.pos) * (3/2), length.out = 10000)
      
      
      segments(x0 = min(grid2[eq(grid2) > cut.pval]), x1 = max(grid1[eq(grid1) > cut.pval]),
               y0 = cut.pval,col = col[i], lty = 2, lwd = 2)
      curve(eq, from = min(md.in.pos), to = min(grid2[eq(grid2) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eq, from = max(grid1[eq(grid1) > cut.pval]), to = max(md.in.neg), add = TRUE, col = col[i], lty = 2, lwd = 2)
    }
  }
  
  # 7 vol, union plot
  if (plot == 7){
    plot(meandiff, -log(pvalue, 10), pch = ".", xlim = xlim, ylim = ylim, main = main,
         xlab = "Mean Difference", ylab = "-log10(p-value)")
    
    for(i in length(cutoff):1){
      a = cutoff[i]
      rstar <- min(r[res_r > a], na.rm = TRUE)
      md.rstar <- meandiff[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      md.rstar.pos <- md.rstar[md.rstar > 0];
      md.rstar.neg <- md.rstar[md.rstar <= 0];
      mlp.rstar <- (-log(pvalue[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)], 10))
      ls.rstar <- ls[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      points(md.rstar, mlp.rstar, col = col[i], cex = 0.5, pch = 16)
      # lines(md.rstar.pos[chull(md.rstar.pos,mlp.rstar)],mlp.rstar[chull(md.rstar.pos,mlp.rstar)])
      # points(md.rstar.pos[chull(md.rstar.pos,mlp.rstar)],mlp.rstar[chull(md.rstar.pos,mlp.rstar)],
      #        cex=1,col="red")
      # lines(md.rstar.neg[chull(md.rstar.neg,mlp.rstar)],mlp.rstar[chull(md.rstar.neg,mlp.rstar)])
      cut.pval <- min(mlp.rstar)
      cut.md.pos <- min(md.rstar.pos, 100);
      cut.md.neg <- max(md.rstar.neg, -100);
      cut.ls <- min(ls[fdr.ls < rstar & fdr.t$lfdr < rstar & ls > quantile(ls, ls.trc)]) ## Cut point;
      
      
      pval.rstar <- pvalue[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      tstat.a <- Tval[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      
      # cut.ls.index<-which.min(ls[fdr.ls>rstar&fdr.t$lfdr<rstar])
      # points(md.rstar[42],mlp.rstar[42],col="red",cex=0.5)
      
      # points(cut.md.pos,cut.pval,cex=1)
      # points(cut.md.neg,cut.pval,cex=1)
      
      eq = function(x){-log(2 - 2 * pt(abs(x)/exp(cut.ls), df = g1 + g2 - 2), 10)}
      
      grid1 <- seq(min(meandiff), 0, length.out = 1000)
      grid2 <- seq(0, max(meandiff), length.out = 1000)
      
      segments(x0 = -1,x1 = max(grid1[eq(grid1) > cut.pval]), y0 = cut.pval, col = col[i], lty = 2, lwd = 2)
      segments(x0 = min(grid2[eq(grid2) > cut.pval]), x1 = 100, y0 = cut.pval, col = col[i], lty = 2, lwd = 2)
      curve(eq, from = min(grid2[eq(grid2) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eq, to = max(grid1[eq(grid1) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
    }
  }
  
  # 8 vol, union lines
  if (plot == 8){
    for(i in length(cutoff):1){
      a = cutoff[i]
      rstar <- min(r[res_r > a], na.rm = TRUE)
      md.rstar <- meandiff[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      md.rstar.pos <- md.rstar[md.rstar > 0];
      md.rstar.neg <- md.rstar[md.rstar <= 0];
      mlp.rstar <- (-log(pvalue[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)], 10))
      ls.rstar <- ls[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      points(md.rstar, mlp.rstar, col = col[i], cex = 0.5, pch = 16)
      # lines(md.rstar.pos[chull(md.rstar.pos,mlp.rstar)],mlp.rstar[chull(md.rstar.pos,mlp.rstar)])
      # points(md.rstar.pos[chull(md.rstar.pos,mlp.rstar)],mlp.rstar[chull(md.rstar.pos,mlp.rstar)],
      #        cex=1,col="red")
      # lines(md.rstar.neg[chull(md.rstar.neg,mlp.rstar)],mlp.rstar[chull(md.rstar.neg,mlp.rstar)])
      cut.pval <- min(mlp.rstar)
      cut.md.pos <- min(md.rstar.pos, 100);
      cut.md.neg <- max(md.rstar.neg, -100);
      cut.ls <- min(ls[fdr.ls < rstar & fdr.t$lfdr < rstar & ls > quantile(ls, ls.trc)]) ## Cut point;
      
      
      pval.rstar <- pvalue[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      tstat.a <- Tval[fdr.t$lfdr < rstar & fdr.ls < rstar & ls > quantile(ls, ls.trc)]
      
      # cut.ls.index<-which.min(ls[fdr.ls>rstar&fdr.t$lfdr<rstar])
      # points(md.rstar[42],mlp.rstar[42],col="red",cex=0.5)
      
      # points(cut.md.pos,cut.pval,cex=1)
      # points(cut.md.neg,cut.pval,cex=1)
      
      eq = function(x){-log(2 - 2 * pt(abs(x)/exp(cut.ls), df = g1 + g2 - 2), 10)}
      
      grid1 <- seq(min(meandiff), 0, length.out = 1000)
      grid2 <- seq(0, max(meandiff), length.out = 1000)
      
      segments(x0 = -1,x1 = max(grid1[eq(grid1) > cut.pval]), y0 = cut.pval, col = col[i], lty = 2, lwd = 2)
      segments(x0 = min(grid2[eq(grid2) > cut.pval]), x1 = 100, y0 = cut.pval, col = col[i], lty = 2, lwd = 2)
      curve(eq, from = min(grid2[eq(grid2) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
      curve(eq, to = max(grid1[eq(grid1) > cut.pval]), add = TRUE, col = col[i], lty = 2, lwd = 2)
    }
  }

  
}

# a <- read.csv("omija.csv")
# idx1 <- 1:27
# idx2 <- -1
# 
# result <- yrkim(a, idx1, idx2)
# result1 <- result[[1]]
# 
# kim_plot(result, plot = 5)
