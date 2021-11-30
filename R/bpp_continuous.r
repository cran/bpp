bpp_continuous <- function(prior = c("normal", "flat"), successmean, stDev, n1, n2, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  finalSE <- sqrt(stDev ^ 2 / n1 + stDev ^ 2 / n2)
  
  if (prior == "normal"){res <- bpp(prior = "normal", successmean = -successmean, 
                                    finalSE = finalSE, priormean = -priormean, 
                                    priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp(prior = "flat", successmean = -successmean, 
                                  finalSE = finalSE, priormean = -priormean, 
                                  width = inp$width, height = inp$height)}
  return(res)  
  
}
