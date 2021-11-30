interval_toIntegrate <- function(x, prior = c("normal", "flat"), interimSE, finalSE, successmean, IntEffBoundary, IntFutBoundary, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  ## compute first term in standard bpp formula:
  pp <- post_power(x = x, interimSE = interimSE, finalSE = finalSE, successmean = successmean, 
                   IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary)
  
  if (prior == "normal"){
    qupdate_norm <- integrate(interval_posterior_nominator, lower = -Inf, upper = Inf, prior = "normal", IntEffBoundary = IntEffBoundary, 
                              IntFutBoundary = IntFutBoundary, interimSE = interimSE, priormean = priormean, priorsigma = inp$priorsigma, 
                              subdivisions = 300)$value
    tmp <- interval_posterior_nominator(x, prior = "normal", IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, 
                                        interimSE = interimSE, priormean = priormean, priorsigma = inp$priorsigma)
  }
  
  if (prior == "flat"){
    qupdate_norm <- integrate(interval_posterior_nominator, lower = -Inf, upper = Inf, prior = "flat", IntEffBoundary = IntEffBoundary, 
                              IntFutBoundary = IntFutBoundary, interimSE = interimSE, priormean = priormean, width = inp$width, 
                              height = inp$height, subdivisions = 300)$value
    tmp <- interval_posterior_nominator(x, prior = "flat", IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, 
                                        interimSE = interimSE, priormean = priormean, width = inp$width, height = inp$height)
  }
  
  res <- pp * tmp / qupdate_norm
  return(res)
}
