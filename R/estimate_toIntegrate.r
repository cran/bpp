estimate_toIntegrate <- function(x, prior = c("normal", "flat"), successmean, finalSE, interimmean, interimSE, priormean, 
                                 propA = 0.5, ...){
  
  # list input arguments
  inp <- list(...)
  
  fac <- (propA * (1 - propA)) ^ (-1)
  IntEvents <- fac / (interimSE ^ 2)
  TotEvents <- fac / (finalSE ^ 2)
  n2 <- TotEvents - IntEvents
  
  # posterior power (--> prior independent)
  postpower <- pnorm((successmean * TotEvents - interimmean * IntEvents - x * n2) / sqrt(fac * n2))
  
  if (prior == "normal"){res <- postpower * estimate_posterior(x, prior = "normal", interimmean, interimSE, priormean, priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- postpower * estimate_posterior(x, prior = "flat", interimmean, interimSE, priormean, width = inp$width, height = inp$height)}
  
  return(res)  
}
