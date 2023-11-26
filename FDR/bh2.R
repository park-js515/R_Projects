bhm <- function(p_value, q_value = c(0.05), plot = F, main = "B-H plot", xlim = NULL, ylim = NULL){
  n1 <- length(p_value)
  n2 <- length(q_value)
  ord <- order(p_value)
  sp <- sort(p_value)
  
  data <- matrix(0, nrow = n1, ncol = 4)
  data[, 2:3] <- cbind(sp, order(p_value))
  la <- c()
  a <- c()
  
  if (length(q_value) == 1){
    rej <- list(q_value = q_value, rej_n = NA, rej1 = NA, order = ord)
  }
  else if (length(q_value) == 2){
    rej <- list(q_value = q_value, rej_n = NA, rej1 = NA, rej2 = NA, order = ord)
  }
  else {
    rej <- list(q_value = q_value, rej_n = NA, rej1 = NA, rej2 = NA, rej3 = NA, order = ord)
  }
  
  qn <- paste0("rej_", q_value)
  names(rej)[3:(2 + length(q_value))] <- qn
  
  for (i in 1:n2){
    cat("run with q_value :", q_value[i], "\n")
    
    for (j in 1:n1){
      data[j, c(1, 4)] <- cbind(j, (j/n1)* q_value[i])
    }
    
    if(sum(data[, 2] < data[, 4]) == 0) {
      la[i] <- 0
    } 
    else {
      la[i] <- max(data[which((data[, 2] <= data[, 4]) == 1), 1])
      rej[[2 + i]] <- sort(data[1:la[i], 3])
    }
  }
  
  rej[[2]] <- la
  
  z <- rej
  
  cat("\nBH result : \n\n")
  
  
  if (plot[1] != F){
    plot(1:n1, sort(p_value), xlab = "# of p_value", ylab = "p_value", main = main, xlim = xlim, ylim = ylim,
         pch = 1, cex = 0.5)

    for (k in plot){
      # bh line
      lines(c(0, n1), c(0, q_value[k]), col = adjustcolor("red", 0.5), lwd = 2)
      text(n1 - n1 %/% 10, q_value[k], paste("q =", q_value[k]), pos = 1, cex = 0.75)
      
      # vertical line
      lines(c(la[k], la[k]), c(0, 1), lty = 4)
      text(la[k], 0.4 + 0.2*(k - 1), paste("# :", la[k]), srt = -90, cex = 0.75, pos = 4)
    }
  }
  
  return(z)

}


# source("functions.R")
# x <- read.csv("omija.csv")
# x2 <- ftn1(x, 1:27, -1)
# x3 <- ftn2(x2, 27, 30)
# x4 <- runif(100)
# 
# bh1 <- bhm(x3, q_value = c(0.05, 0.1, 0.15), plot = 1:3)
# bh2 <- bhm(x4, q_value = c(0.5, 0.6, 0.7), plot = c(1, 3))


# library(shiny)
# library(locfdr)

# ui <- fluidPage(
#   plotOutput("plot1", width = "550px", height = "550px")
# )
# 
# server <- function(input, output){
# 
#   output$plot1 <- renderPlot({
#     data <- reactive(runif(100))
#     bhm(data(), q_value = c(0.2), plot = 1)
#   })
# }
# 
# shinyApp(ui, server)