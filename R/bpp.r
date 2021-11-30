bpp <- function(prior = c("normal", "flat"), successmean, finalSE, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  if (prior == "normal"){res <- pnorm((successmean - priormean) / sqrt(finalSE ^ 2 + inp$priorsigma ^ 2))}
  if (prior == "flat"){
    toIntegrate <- function(x, successmean, finalSE, priormean, width, height){return(pnorm((successmean - x) / finalSE) * dUniformNormalTails(x, priormean, width, height))}
    res <- integrate(toIntegrate, lower = -Inf, upper = Inf, successmean = successmean, finalSE = finalSE, priormean = priormean, 
                     width = inp$width, height = inp$height, subdivisions = 300)$value
  }
  
  return(res)
}  
