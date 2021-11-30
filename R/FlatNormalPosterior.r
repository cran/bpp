FlatNormalPosterior <- function(x, successmean, finalSE, interimmean, interimSE, priormean, width, height){
  pow <- pnorm((successmean - x) / finalSE)
  post <- estimate_posterior(x, prior = "flat", interimmean = interimmean, interimSE = interimSE, priormean = priormean, width = width, height = height)
  res <- pow * post
  return(res)  
}

