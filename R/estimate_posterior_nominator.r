estimate_posterior_nominator <- function(x, prior = c("normal", "flat"), datamean, datasigma, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  #res1 <- dnorm(x, mean = datamean, sd = datasigma) * dnorm(x, mean = priormean, sd = priorsigma)
  if (prior == "normal"){res <- exp(- 1 / (2 * datasigma ^ 2) * (x - datamean) ^ 2) * dnorm(x, mean = priormean, sd = inp$priorsigma)}
  if (prior == "flat"){res <- dnorm(x, mean = datamean, sd = datasigma) * dUniformNormalTails(x, mu = priormean, width = inp$width, height = inp$height)}
  
  return(res)
}