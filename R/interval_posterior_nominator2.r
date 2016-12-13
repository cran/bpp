interval_posterior_nominator2 <- function(x, prior = "normal", IntEffBoundary, IntFutBoundary, datasigma, priormean, ...){
  # list input arguments
  inp <- list(...)

  res1 <- rep(NA, length(x))
  for (i in 1:length(x)){
    covmat2 <- rbind(c(datasigma[1], datasigma[2]), 
                     c(datasigma[2], datasigma[2])) ^ 2
    t2 <- as.numeric(pmvnorm(lower = IntEffBoundary, upper = IntFutBoundary, mean = c(x[i], x[i]), sigma = covmat2))
    if (prior == "normal"){res1[i] <- t2 * dnorm(x[i], mean = priormean, sd = inp$priorsigma)}
  }
  
  return(res1)
}

