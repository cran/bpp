bpp_1interim <- function(prior = c("normal", "flat"), interimSE, finalSE, successmean, IntEffBoundary, 
                         IntFutBoundary, IntFix, priormean, propA = 0.5, thetas, ...){
  
  # list input arguments
  inp <- list(...)
  
  ## ------------------------------------------
  ## posterior power:
  ## ------------------------------------------
  pp <- post_power(x = thetas, interimSE = interimSE, finalSE = finalSE, successmean = successmean, 
                   IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary)

  ## ------------------------------------------
  ## normal prior
  ## ------------------------------------------
  if (prior == "normal"){
    
    # initial BPP
    bpp0 <- bpp(prior = "normal", successmean = successmean, finalSE = finalSE, priormean = priormean, priorsigma = inp$priorsigma)
  
    ## compute BPP after not stopping at interim:
    bpp3 <- integrate(Vectorize(interval_toIntegrate), lower = -Inf, upper = Inf, prior = "normal", interimSE = interimSE, 
                    finalSE = finalSE, successmean = successmean, IntEffBoundary = IntEffBoundary, 
                    IntFutBoundary = IntFutBoundary, priormean = priormean, priorsigma = inp$priorsigma, subdivisions = 300)$value

    ## posterior density after not stopping at interim, interval knowledge
    qupdate_norm3 <- integrate(interval_posterior_nominator, lower = -Inf, upper = Inf, prior = "normal", 
                             IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, interimSE = interimSE, 
                             priormean = priormean, priorsigma = inp$priorsigma, subdivisions = 300)$value
    post3 <- rep(NA, length(thetas))
    for (i in 1:length(thetas)){
      post3[i] <- interval_posterior_nominator(x = thetas[i], prior = "normal", IntEffBoundary = IntEffBoundary, 
                                                    IntFutBoundary = IntFutBoundary, interimSE = interimSE, 
                                                    priormean = priormean, priorsigma = inp$priorsigma)
    }
    post3 <- post3 / qupdate_norm3
  
    ## compute BPP after update, exact knowledge
    bpp.fix <- rep(NA, length(IntFix))
    for (i in 1:length(bpp.fix)){
      bpp.fix[i] <- integrate(Vectorize(estimate_toIntegrate), lower = -Inf, upper = Inf, prior = "normal",
                                successmean = successmean, finalSE = finalSE, interimmean = IntFix[i], 
                                interimSE = interimSE, priormean = priormean, propA = propA, 
                                priorsigma = inp$priorsigma, subdivisions = 300)$value
    }
  
    ## posterior density after not stopping at interim, exact knowledge
    ## on grid of thetas and IntFix
    post4 <- matrix(NA, ncol = length(IntFix), nrow = length(thetas))
    for (j in 1:length(IntFix)){
      qupdate_norm4.j <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, 
                                 prior = "normal", interimmean = IntFix[j], interimSE = interimSE, 
                                 priormean = priormean, priorsigma = inp$priorsigma, subdivisions = 300)$value
    post4.j <- rep(NA, length(thetas))
    for (i in 1:length(thetas)){
      post4.j[i] <- estimate_posterior_nominator(thetas[i], prior = "normal", interimmean = IntFix[j], 
                                 interimSE = interimSE, priormean = priormean, priorsigma = inp$priorsigma)
    }
    post4.j <- post4.j / qupdate_norm4.j
    post4[, j] <- post4.j
  }
  
  }
  
  ## ------------------------------------------
  ## flat prior
  ## ------------------------------------------
  if (prior == "flat"){
    
    # initial BPP
    bpp0 <- bpp(prior = "flat", successmean = successmean, finalSE = finalSE, priormean = priormean, 
                width = inp$width, height = inp$height)
    
    ## compute BPP after not stopping at interim:
    bpp3 <- integrate(Vectorize(interval_toIntegrate), lower = -Inf, upper = Inf, prior = "flat", interimSE = interimSE, 
                      finalSE = finalSE, successmean = successmean, IntEffBoundary = IntEffBoundary, 
                      IntFutBoundary = IntFutBoundary, priormean = priormean, width = inp$width, height = inp$height, 
                      subdivisions = 300)$value
    
    ## posterior density after not stopping at interim, interval knowledge
    qupdate_norm3 <- integrate(interval_posterior_nominator, lower = -Inf, upper = Inf, prior = "flat", 
                               IntEffBoundary = IntEffBoundary, IntFutBoundary = IntFutBoundary, interimSE = interimSE, 
                               priormean = priormean, width = inp$width, height = inp$height, subdivisions = 300)$value
    post3 <- rep(NA, length(thetas))
    for (i in 1:length(thetas)){
      post3[i] <- interval_posterior_nominator(x = thetas[i], prior = "flat", IntEffBoundary = IntEffBoundary, 
                                               IntFutBoundary = IntFutBoundary, interimSE = interimSE, 
                                               priormean = priormean, width = inp$width, height = inp$height)
    }
    post3 <- post3 / qupdate_norm3
    
    ## compute BPP after update, exact knowledge
    bpp.fix <- rep(NA, length(IntFix))
    for (i in 1:length(bpp.fix)){
      bpp.fix[i] <- integrate(Vectorize(estimate_toIntegrate), lower = -Inf, upper = Inf, prior = "flat",
                              successmean = successmean, finalSE = finalSE, interimmean = IntFix[i], 
                              interimSE = interimSE, priormean = priormean, propA = propA, 
                              width = inp$width, height = inp$height, subdivisions = 300)$value
    }
    
    ## posterior density after not stopping at interim, exact knowledge
    ## on grid of thetas and IntFix
    post4 <- matrix(NA, ncol = length(IntFix), nrow = length(thetas))
    for (j in 1:length(IntFix)){
      qupdate_norm4.j <- integrate(estimate_posterior_nominator, lower = -Inf, upper = Inf, 
                                   prior = "flat", interimmean = IntFix[j], interimSE = interimSE, 
                                   priormean = priormean, width = inp$width, height = inp$height, subdivisions = 300)$value
      post4.j <- rep(NA, length(thetas))
      for (i in 1:length(thetas)){
        post4.j[i] <- estimate_posterior_nominator(thetas[i], prior = "flat", interimmean = IntFix[j], 
                                                   interimSE = interimSE, priormean = priormean, 
                                                   width = inp$width, height = inp$height)
      }
      post4.j <- post4.j / qupdate_norm4.j
      post4[, j] <- post4.j
    }
    
  } 
  
  
  ## ------------------------------------------
  ## output
  ## ------------------------------------------
  res <- list("initial BPP" = bpp0, 
              "conditional power interval" = pp,
              "BPP after not stopping at interim interval" = bpp3, 
              "BPP after not stopping at interim exact" = rbind(IntFix, bpp.fix),
              "posterior density exact" = post4,
              "posterior density interval" = post3
  )
  return(res)
}

