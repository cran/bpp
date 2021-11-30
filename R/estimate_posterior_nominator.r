estimate_posterior_nominator <- function(x, prior = c("normal", "flat"), interimmean, interimSE, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  #res1 <- dnorm(x, mean = interimmean, sd = interimSE) * dnorm(x, mean = priormean, sd = priorsigma)
  if (prior == "normal"){res <- exp(- 1 / (2 * interimSE ^ 2) * (x - interimmean) ^ 2) * dnorm(x, mean = priormean, sd = inp$priorsigma)}
  if (prior == "flat"){res <- dnorm(x, mean = interimmean, sd = interimSE) * dUniformNormalTails(x, mu = priormean, width = inp$width, height = inp$height)}
  
  return(res)
}
