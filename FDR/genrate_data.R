# 2 null, 2 alter
# return (data, liklihood of f, liklihood of f0, true fdr, plot)
# n : data size, p : null ratio, p0 : ratio of N(mean1, var1) [that means (1 - p0) is ratio of N(mean2, var2)]
# eta : ratio of left side alter 
# m1 : mean1, v1 : var1, m2 : mean2, var2 : var2, df : df of alter ~ t(df), adj : adjust value of alternative dist
# plot : if T, draw histogram, & density of f(generated values)


rdat <- function(n = 8000, p = 0.9, p0 = 0.6, eta = 0.5,
                 m1 = 0, v1 = 0.1, m2 = 0, v2 = 0.02, df = 20, adj = 2, plot = T){
  
  #  data 
  n0 <- sum(rbinom(n, 1, p))
  n00 <- sum(rbinom(n0, 1, p0))
  n01 <- n0 - n00
  
  n1 <- n - n0
  n10 <- sum(rbinom(n1, 1, eta))
  n11 <- n1 - n10
  
  nulltype <- rep(c(0, 1), times = c(n0, n1))
  side <- rep(c(0, 1, 0, 1), times = c(n00, n01, n10, n11))
  value <- c(rnorm(n00, m1, sqrt(v1)), rnorm(n01, m2, sqrt(v2)), (rt(n10, df) - adj), (rt(n11, df) + adj))
  
  data <- data.frame(nulltype, side, value)
  
  # liklihood of f
  vr <- range(value)
  vr_seq <- seq(vr[1], vr[2], 0.01)
  
  f <- p * p0 * dnorm(vr_seq, m1, sqrt(v1)) + p * (1 - p0) * dnorm(vr_seq, m2, sqrt(v2)) +
    (1 - p) * eta * dt(vr_seq - adj, df) + (1 - p) * (1 - eta) * dt(vr_seq + adj, df)
  f0 <- p0 * dnorm(vr_seq, m1, sqrt(v1)) + (1- p0) * dnorm(vr_seq, m2, sqrt(v2))
  f1 <- eta * dt(vr_seq - adj, df) + (1 - eta) * dt(vr_seq + adj, df)
  true_fdr <- p*f0/f
  
  stats <- data.frame(stat = vr_seq, f, f0, f1, true_fdr)
  
  # plot 
  if (plot){
    hist(value, freq = F, nclass = 300, col = 0, border = adjustcolor(1, 0.5), main = "density of f",
         xlab = "stat")
    lines(vr_seq, f, type = "l", lty = 2)
  }
  
  return(list(data = data, stats = stats))
}

rdata <- rdat()

data <- rdata$data
stats <- rdata$stats

hist(data$value, freq = F, nclass = 300, col = 0, border = adjustcolor(1, 0.5),
     main = "density of p0*f0", xlab = "stat")
lines(stats$stat, stats$f0 * 0.9, type = "l", lty = 4, col = 2, lwd  = 2)
