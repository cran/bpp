pUniformNormalTails <- function(x, mu, width, height){

  res <- rep(NA, length(x))
  
  for (i in 1:length(res)){
    res[i] <- integrate(dUniformNormalTails, lower = -Inf, upper = x, mu = mu, width = width, height = height)$value
    }
  
  return(res)
}