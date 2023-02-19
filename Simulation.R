
# Codes

library(parallel)

rm(list = ls())

n <- 1e4

sim_n <- 1e5

r_func <- function(pd){
  r <- 0.12 * (1 - exp(-50 * pd)) / (1 - exp(-50)) + 0.24 * (1 - (1 - exp(-50 * pd)) / (1 - exp(-50)))
  return(r)
}

b_func <- function(pd){
  b = (0.11852 - 0.05478 * log(pd)) ^ 2
  return(b)
}

rw_func <- function(pd, lgd, r, b, m){
  rw = (lgd * pnorm(1 / sqrt(1 - r) * qnorm(pd) + sqrt(r /(1 - r)) * qnorm(0.999)) - pd * lgd) * (1 - 1.5 * b) ^ (-1) * (1 + (m - 2.5) * b) * 1.06 * 12.5
  return(rw)
}

data <- data.frame(id = paste0('client_', seq(1, n, 1)),
                   pd = round(runif(n, min = 0.06, max = 0.4), digits = 5),
                   lgd = c(0.45),
                   ead = c(100, round(runif(n - 1, min = 10, max = 80))))


data["r"] <- round(mapply(r_func, data$pd), digits = 6)
data["m"] <- c(2.5)
data["b"] <- round(mapply(b_func, data$pd), digits = 6)
data["rw"] <- round(mapply(rw_func, data$pd, data$lgd, data$r, data$b, data$m), digits = 6)
data["rwa"] <- round(data$rw * data$ead, digits = 2)
data["loss"] <- data$ead * data$lgd


sim_func <- function(i, data){
  
  data["kuszob"] <- qnorm(data$pd)
  
  data["kozos"] <- rep(rnorm(1), dim(data)[1])
  
  data["egyedi"] <- rnorm(dim(data)[1])
  
  data["generalt"] <- (data$r ^ 0.5) * data$kozos + (1-data$r) ^ (0.5) * data$egyedi
  
  return( t(data$kuszob > data$generalt) %*% data$loss )
  
}

cl <- makeCluster(4)
clusterSetRNGStream(cl, iseed = 1234)

loss_vector <- unlist(parLapply(cl, seq(1:sim_n), fun = sim_func, data = data))

stopCluster(cl)

stats <- function(v){
  l <- list()
  l$mean <- mean(v)
  l$median <- median(v)
  l$quantile_95 <- quantile(v, 0.95)
  l$quantile_99 <- quantile(v, 0.99)
  l$sd <- sd(v)
  return(l)
}

stats(loss_vector)
