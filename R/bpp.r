bpp <- function(prior = c("normal", "flat"), successmean, finalsigma, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  if (prior == "normal"){res <- pnorm((successmean - priormean) / sqrt(finalsigma ^ 2 + inp$priorsigma ^ 2))}
  if (prior == "flat"){
    toIntegrate <- function(x, successmean, finalsigma, priormean, width, height){return(pnorm((successmean - x) / finalsigma) * dUniformNormalTails(x, priormean, width, height))}
    res <- integrate(toIntegrate, lower = -Inf, upper = Inf, successmean = successmean, finalsigma = finalsigma, priormean = priormean, width = inp$width, height = inp$height)$value
  }
  
  return(res)
}  