interval_posterior_nominator <- function(x, prior = c("normal", "flat"), IntEffBoundary, IntFutBoundary, datasigma, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  postpower <- (pnorm(IntFutBoundary, mean = x, sd = datasigma) - pnorm(IntEffBoundary, mean = x, sd = datasigma))
  
  if (prior == "normal"){dens <- dnorm(x, mean = priormean, sd = inp$priorsigma)}
  if (prior == "flat"){dens <- dUniformNormalTails(x, mu = priormean, width = inp$width, height = inp$height)}
  
  res <- postpower * dens
  
  return(res)
}  