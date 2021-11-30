interval_posterior_nominator <- function(x, prior = c("normal", "flat"), IntEffBoundary, IntFutBoundary, interimSE, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  postpower <- (pnorm(IntFutBoundary, mean = x, sd = interimSE) - pnorm(IntEffBoundary, mean = x, sd = interimSE))
  
  if (prior == "normal"){dens <- dnorm(x, mean = priormean, sd = inp$priorsigma)}
  if (prior == "flat"){dens <- dUniformNormalTails(x, mu = priormean, width = inp$width, height = inp$height)}
  
  res <- postpower * dens
  
  return(res)
}  
