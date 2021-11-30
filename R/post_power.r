post_power <- function(x, interimSE, finalSE, successmean, IntEffBoundary, IntFutBoundary){
  
  pp <- rep(NA, length(x))
  covmat <- matrix(c(interimSE ^ 2, finalSE ^ 2, finalSE ^ 2, finalSE ^ 2), ncol = 2, byrow = TRUE)
  for (i in 1:length(x)){
    t1 <- as.numeric(pmvnorm(lower = c(IntEffBoundary, -Inf), upper = c(IntFutBoundary, successmean), mean = c(x[i], x[i]), sigma = covmat))
    t2 <- pnorm((IntFutBoundary - x[i]) / interimSE) - pnorm((IntEffBoundary - x[i]) / interimSE)
    if (t1 <= 10^-10){pp[i] <- 0} else {pp[i] <- t1 / t2}
  }

 return(pp) 
}
