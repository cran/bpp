estimate_posterior <- function(x, prior = c("normal", "flat"), datamean, datasigma, priormean, ...){
  
  # list input arguments
  inp <- list(...)
  
  if (prior == "normal"){
    nomi <- estimate_posterior_nominator(x, prior = "normal", datamean, datasigma, priormean, priorsigma = inp$priorsigma)
    deno <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, prior = "normal", datamean = datamean, 
                      datasigma = datasigma, priormean = priormean, priorsigma = inp$priorsigma)$value
  }
  
  if (prior == "flat"){
    nomi <- estimate_posterior_nominator(x, prior = "flat", datamean, datasigma, priormean, width = inp$width, height = inp$height)
    deno <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, prior = "flat", datamean = datamean, 
                      datasigma = datasigma, priormean = priormean, width = inp$width, height = inp$height)$value
  }
  
  res <- nomi / deno
  return(res)
}