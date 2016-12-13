interval_toIntegrate2 <- function(x, prior = "normal", datasigma, finalsigma, successmean, IntEffBoundary, IntFutBoundary, priormean, ...){
  
  # list input arguments
  inp <- list(...)

  res <- rep(NA, length(x))
  
  for (i in 1:length(x)){
  
  ## compute first term in standard BPP formula: 3-dimensional nominator
  covmat1 <- rbind(c(datasigma[1], datasigma[2], finalsigma), 
                   c(datasigma[2], datasigma[2], finalsigma),
                   c(finalsigma,   finalsigma,   finalsigma)) ^ 2
  t1 <- as.numeric(pmvnorm(lower = c(IntEffBoundary, -Inf), upper = c(IntFutBoundary, successmean), 
                           mean = c(x[i], x[i], x[i]), sigma = covmat1))
  
  ## compute first term in standard BPP formula: 2-dimensional denominator
  covmat2 <- rbind(c(datasigma[1], datasigma[2]), 
                   c(datasigma[2], datasigma[2])) ^ 2
  t2 <- as.numeric(pmvnorm(lower = IntEffBoundary, upper = IntFutBoundary, mean = c(x[i], x[i]), sigma = covmat2))
  
  ## compute posterior power
  if (t1 <= 10^-10){pp <- 0} else {pp <- t1 / t2}

  if (prior == "normal"){
        ## compute q_update (x = x)
        qupdate_norm <- integrate(interval_posterior_nominator2, lower = -Inf, upper = Inf, 
                            prior = "normal", IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, 
                            datasigma = datasigma, priormean = priormean, priorsigma = inp$priorsigma)$value
        res[i] <- pp * interval_posterior_nominator2(x[i], prior = "normal", IntEffBoundary = IntEffBoundary, 
                            IntFutBoundary = IntFutBoundary, datasigma = datasigma, priormean = priormean, 
                            priorsigma = inp$priorsigma) / qupdate_norm
  }
  
  }
  return(res)
}









