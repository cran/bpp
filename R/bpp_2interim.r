bpp_2interim <- function(prior = "normal", direction_lower = TRUE, datasigma, finalsigma, successmean, IntEffBoundary, IntFutBoundary, priormean, thetas, ...){

  # list input arguments
  inp <- list(...)

  if (isFALSE(direction_lower)){
    successmean <- -successmean
    priormean <- -priormean
    IntEffBoundary <- -IntEffBoundary
    IntFutBoundary <- -IntFutBoundary
  }
  
  ## ------------------------------------------
  ## normal prior
  ## ------------------------------------------
  if (prior == "normal"){
    
    # initial BPP
    bpp0 <- bpp(prior = "normal", successmean = successmean, finalsigma = finalsigma, priormean = priormean, priorsigma = inp$priorsigma)

    ## compute BPP after not stopping at interim:
    bpp3_2stage <- integrate(interval_toIntegrate2, lower = -Inf, upper = Inf, prior = "normal",
                   datasigma = datasigma, finalsigma = finalsigma, successmean = successmean, 
                   IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, priormean = priormean, 
                   priorsigma = inp$priorsigma)$value

    ## posterior density after not stopping at interim:
    qupdate_norm3_2stage <- integrate(interval_posterior_nominator2, lower = -Inf, upper = Inf, prior = "normal", 
                           IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, datasigma = datasigma, 
                           priormean = priormean, priorsigma = inp$priorsigma)$value

    post3_2stage <- rep(NA, length(thetas))
    for (i in 1:length(thetas)){
        post3_2stage[i] <- interval_posterior_nominator2(thetas[i], IntEffBoundary = IntEffBoundary, 
                               IntFutBoundary = IntFutBoundary, datasigma = datasigma, 
                               priormean = priormean, priorsigma = inp$priorsigma)
    }

    post3_2stage <- post3_2stage / qupdate_norm3_2stage
    }

## generate output
res <- list("initial BPP" = bpp0, "BPP after not stopping at interim interval" = bpp3_2stage, "posterior density interval" = post3_2stage)
return(res)
}
