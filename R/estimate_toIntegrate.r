estimate_toIntegrate <- function(x, prior = c("normal", "flat"), successmean, finalsigma, datamean, datasigma, priormean, 
                                 propA = 0.5, ...){
  
  # list input arguments
  inp <- list(...)
  
  fac <- (propA * (1 - propA)) ^ (-1)
  IntEvents <- fac / (datasigma ^ 2)
  TotEvents <- fac / (finalsigma ^ 2)
  n2 <- TotEvents - IntEvents
  
  # posterior power (--> prior independent)
  postpower <- pnorm((successmean * TotEvents - datamean * IntEvents - x * n2) / sqrt(fac * n2))
  
  if (prior == "normal"){res <- postpower * estimate_posterior(x, prior = "normal", datamean, datasigma, priormean, priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- postpower * estimate_posterior(x, prior = "flat", datamean, datasigma, priormean, width = inp$width, height = inp$height)}
  
  return(res)  
}