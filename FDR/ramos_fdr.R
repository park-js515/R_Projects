# source("real_func.R");source("real_flex.R")

ramos_fdr <- function(Zval, DC = 3, df = 20, bre = 300, type = 0, inicialC = 0.5, Afac = 0.2, lastC = 1.5, trunc = T){
  
  dengenres <- dengen(Zval, DC = DC, bre = bre, df = df, trunc = trunc, type = type); # Common parameters?
  aax <- dengenres[, 1];
  den <- dengenres[, 2];
  remanres <- dengenres[, 3];
  initialc <- inicialC # initial c # R
  initialres <- simn(Zval, initialc, aax, den, remanres)
  
  nmaxima <- 2; # 고정값 
  afac <- 0; # R
  ends <- lastC # last c # R
  
  # 정확히 무엇을 하는지 알기는 어려움. a flex를 실행하는 것이라 생각 (4차 회귀를 이용)
  while(nmaxima > 1){
    afac <- afac + Afac;
    initialc<-(0.8 + afac) * sqrt(min(initialres[4], initialres[6])); # use small variance to sigma1
    c <- cpick(Zval, c = initialc, inc = bre/2, end = ends);
    result <- simnstep1(Zval, c, aax, den);
    p4mod <- lm(result[, 2] ~ result[, 1] + I(result[, 1]^2) + I(result[, 1]^3) + I(result[, 1]^4));
    p4coef <- rev(as.vector(p4mod$coefficients));
    p3coef <- c(4*p4coef[1], 3*p4coef[2], 2*p4coef[3], p4coef[4]);
    p3roots <- cubic(p3coef);
    p2coef <- c(3*p3coef[1], 2*p3coef[2], p3coef[3]);
    # plot(result[,1],result[,2]);
    # curve(p4func(x,c=p4coef),add=TRUE);
    testmat <- rbind(p3roots, p2func(p3roots, c = p2coef));
    nmaxima <- 0;
    for(j in 1:3){
      if(is.numeric(testmat[1, j]) & as.numeric(testmat[1, j]) > min(result[, 1]) & as.numeric(testmat[1, j]) < max(result[, 1]) & as.numeric(testmat[2, j]) < 0){
        nmaxima <- nmaxima + 1;}
    }
    
    if(p3func(x = c[1],c = p3coef) < 0 & p3func(x=c[2],c=p3coef) < 0 & p3func(x = c[3], c = p3coef) < 0 & p3func(x = c[4],c = p3coef) < 0){
      nmaxima <- nmaxima + 1;}
    
    cat(afac,"\n")
  }
  if(max(result[, 2]) - max(result[, 2][result[, 2] != max(result[, 2])] ) > 100){result <- result[result[, 2] != max(result[, 2]), ];}
  
  Ramos_res <- bestfind(result);
  Ramos_fdr <- mylocfdr2(Zval, Ramos_res, aax, den, remanres);
  
  out <- list(Ramos_res = Ramos_res, Ramos_fdr = Ramos_fdr, aax = aax, den = den)
  
  cat("done!\n")
  return(out)
}

# ramos_plot
# plot = 1 : pi0*f0 (use lines function)
# plot = 2 : pi0*f0 (use plot function + ...)
# plot = 3 : ramos fdr line (use lines function)
# plot = 4 : ramos dfr line (use plot function + ...)
ramos_plot <- function(ramos_res, plot = 1, main = "", xlab = "", ylab = "", xlim = NULL, ylim = NULL){
  Ramos_res <- ramos_res$Ramos_res
  Ramos_fdr <- ramos_res$Ramos_fdr
  aax <- ramos_res$aax
  den <- ramos_res$den
  
  # pi0*f0
  F0_R <- Ramos_fdr[1, 3] * (dnorm(aax, Ramos_res[3], sqrt(Ramos_res[4])) * Ramos_res[7] +
                               dnorm(aax, Ramos_res[5], sqrt(Ramos_res[6])) * (1 - Ramos_res[7]))
  # fdr
  RF <- pmin(F0_R/den, 1)
  
  if (plot == 1){
    lines(aax, F0_R, col = 4, lty = 4, lwd = 1.5)
    }
  else if (plot == 2){
    plot(aax, F0_R, type = "l", col = 4, lty = 4, lwd = 1.5, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim)
    }
  else if (plot == 3){
    lines(aax, RF, col = 4, lty = 4, lwd = 1.5)
    }
  else {
    plot(aax, RF, col = 4, type = "l", lty = 4, lwd = 1.5, main = main, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim)
    }
  }



# # simulation
# sim1 <- rdata(n = 8000, m0 = 0, m1 = 0, v0 = 0.02, v1 = 0.1, p = 0.6, altp = 0.1)
# 
# # summary(sim1)
# # sim1
# # sum(sim1[, 3])
# 
# f00 <- subset(sim1, sim1[, 1] == 0 & sim1[, 3] == 0)
# f01 <- subset(sim1, sim1[, 1] == 1 & sim1[, 3] == 0)
# f10 <- subset(sim1, sim1[, 1] == 0 & sim1[, 3] == 1)
# f11 <- subset(sim1, sim1[, 1] == 1 & sim1[, 3] == 1)
# #
# # n <- c(dim(f00)[1], dim(f01)[1], dim(f10)[1], dim(f11)[1])
# # n
# #
# # f00
# 
# sim11 <- sim1[, 2]
# 
# idx <- order(sim11) # 저장할 필요가 있다...
# Zval <- sim11[idx]
# DC <- 3 # C (어떻게 해야하는지 생각해야한다. 초기값은 어떻게 설정하지?)
# bre <- 300 # C
# df <- 10 # C
# trunc <- T
# 
# ramos_res <- ramos_fdr(Zval, DC = DC, bre = bre, df = 10, inicialC = 0.5, Afac = 0, lastC = 2, trunc = trunc)
# ramos_plot(ramos_res, plot = 4)
