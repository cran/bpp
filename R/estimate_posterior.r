estimate_posterior <- function(x, prior = c("normal", "flat"), interimmean, interimSE, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  if (prior == "normal"){
    nomi <- estimate_posterior_nominator(x, prior = "normal", interimmean, interimSE, priormean, priorsigma = inp$priorsigma)
    deno <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, prior = "normal", interimmean = interimmean, 
                      interimSE = interimSE, priormean = priormean, priorsigma = inp$priorsigma, subdivisions = 300)$value
  }
  
  if (prior == "flat"){
    nomi <- estimate_posterior_nominator(x, prior = "flat", interimmean, interimSE, priormean, width = inp$width, height = inp$height)
    deno <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, prior = "flat", interimmean = interimmean, 
                      interimSE = interimSE, priormean = priormean, width = inp$width, height = inp$height, subdivisions = 300)$value
  }
  
  res <- nomi / deno
  return(res)
}
