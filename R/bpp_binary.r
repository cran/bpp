bpp_binary <- function(prior = c("normal", "flat"), successdelta, pi1, n1,
                       pi2, n2, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  finalSE <- sqrt(pi2 * (1 - pi2) / n2 + pi1 * (1 - pi1) / n1)
  
  if (prior == "normal"){res <- bpp(prior = "normal", successmean = -successdelta, 
                                    finalSE = finalSE, priormean = -priormean, 
                                    priorsigma = inp$priorsigma)}
  if (prior == "flat"){res <- bpp(prior = "flat", successmean = -successdelta, 
                                  finalSE = finalSE, priormean = -priormean, 
                                  width = inp$width, height = inp$height)}
  return(res)  
  
}
