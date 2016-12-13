FlatNormalPosterior <- function(x, successmean, finalsigma, datamean, datasigma, priormean, width, height){
  pow <- pnorm((successmean - x) / finalsigma)
  post <- estimate_posterior(x, prior = "flat", datamean = datamean, datasigma = datasigma, priormean = priormean, width = width, height = height)
  res <- pow * post
  return(res)  
}

