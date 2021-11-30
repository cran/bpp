bpp_t2e <- function(prior = c("normal", "flat"), successHR, d, propA = 0.5,
                    priorHR, ...){
  
  # list input arguments
  inp <- list(...)
  fac <- (propA * (1 - propA)) ^ (-1)
  finalSE <- sqrt(fac / d)
  
  if (prior == "normal"){res <- bpp(prior = "normal", successmean = log(successHR), finalSE = finalSE, 
                                    priormean = log(priorHR), priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp(prior = "flat", successmean = log(successHR), finalSE = finalSE, 
                                  priormean = log(priorHR), width = inp$width, height = inp$height)}
  return(res)  
  
}
