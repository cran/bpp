bpp_1interim_binary <- function(prior = c("normal", "flat"), successdelta, pi1, n1,
                                pi2, n2, IntEffBoundary, IntFutBoundary, IntFix, priormean, 
                                propA = 0.5, thetas, ...){
  
  # list input arguments
  inp <- list(...)
  
  # standard errors
  se_int <- sqrt(pi1 *(1 - pi1) / n1[1] + pi2 * (1 - pi2) ^ 2 / n2[1])
  finalSE <- sqrt(pi1 * (1 - pi1) / n1[2] + pi2 * (1 - pi2) / n2[2])
  
  if (prior == "normal"){res <- bpp_1interim(prior = "normal", interimSE = se_int, 
                                             finalSE = finalSE, successmean = -successdelta, 
                                             IntEffBoundary = -IntEffBoundary, IntFutBoundary = -IntFutBoundary, 
                                             IntFix = -IntFix, propA = propA, thetas = -thetas, 
                                             priormean = -priormean, priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp_1interim(prior = "flat", interimSE = se_int, 
                                           finalSE = finalSE, successmean = -successdelta, 
                                           IntEffBoundary = -IntEffBoundary, IntFutBoundary = -IntFutBoundary, 
                                           IntFix = -IntFix, propA = propA, thetas = -thetas, 
                                           priormean = -priormean, width = inp$width, height = inp$height)}
  return(res)  
}
