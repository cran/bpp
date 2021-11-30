bpp_1interim_t2e <- function(prior = c("normal", "flat"), successHR, d,
                             IntEffBoundary, IntFutBoundary, IntFixHR, 
                             priorHR, propA = 0.5, thetas, ...){
  
  # list input arguments
  inp <- list(...)
  fac <- (propA * (1 - propA)) ^ (-1)
  interimSE <- sqrt(fac / d[1])
  finalSE <- sqrt(fac / d[2])
  
  if (prior == "normal"){res <- bpp_1interim(prior = "normal", interimSE = interimSE, 
                                             finalSE = finalSE, successmean = log(successHR), 
                                             IntEffBoundary = log(IntEffBoundary), 
                                             IntFutBoundary = log(IntFutBoundary), IntFix = log(IntFixHR), 
                                             propA = propA, thetas = log(thetas), 
                                             priormean = log(priorHR), priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp_1interim(prior = "flat", interimSE = interimSE, 
                                           finalSE = finalSE, successmean = log(successHR), 
                                           IntEffBoundary = log(IntEffBoundary), 
                                           IntFutBoundary = log(IntFutBoundary), 
                                           IntFix = log(IntFixHR), 
                                           propA = propA, thetas = log(thetas), 
                                           priormean = log(priorHR), width = inp$width, height = inp$height)}
  return(res)    
}
