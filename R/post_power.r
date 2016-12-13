post_power <- function(x, datasigma, finalsigma, successmean, IntEffBoundary, IntFutBoundary){
  
  pp <- rep(NA, length(x))
  covmat <- matrix(c(datasigma ^ 2, finalsigma ^ 2, finalsigma ^ 2, finalsigma ^ 2), ncol = 2, byrow = TRUE)
  for (i in 1:length(x)){
    t1 <- as.numeric(pmvnorm(lower = c(IntEffBoundary, -Inf), upper = c(IntFutBoundary, successmean), mean = c(x[i], x[i]), sigma = covmat))
    t2 <- pnorm((IntFutBoundary - x[i]) / datasigma) - pnorm((IntEffBoundary - x[i]) / datasigma)
    if (t1 <= 10^-10){pp[i] <- 0} else {pp[i] <- t1 / t2}
  }

 return(pp) 
}