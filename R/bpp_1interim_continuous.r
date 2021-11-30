bpp_1interim_continuous <- function(prior = c("normal", "flat"), successmean, stDev, 
                                    n1, n2, IntEffBoundary, IntFutBoundary, IntFix, 
                                    priormean, propA = 0.5, thetas, ...){
  
  # list input arguments
  inp <- list(...)
  se <- sqrt(stDev ^ 2 / n1 + stDev ^ 2 / n2)
  
  if (prior == "normal"){res <- bpp_1interim(prior = "normal", interimSE = se[1], 
                                             finalSE = se[2], successmean = -successmean, 
                                             IntEffBoundary = -IntEffBoundary, 
                                             IntFutBoundary = -IntFutBoundary, IntFix = -IntFix, 
                                             propA = propA, thetas = -thetas, priormean = -priormean, 
                                             priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp_1interim(prior = "flat", interimSE = se[1], 
                                           finalSE = se[2], successmean = -successmean, 
                                           IntEffBoundary = -IntEffBoundary, 
                                           IntFutBoundary = -IntFutBoundary, IntFix = -IntFix, 
                                           propA = propA, thetas = -thetas, priormean = -priormean, 
                                           width = inp$width, height = inp$height)}
  return(res)  
}
