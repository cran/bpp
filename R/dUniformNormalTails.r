dUniformNormalTails <- function(x, mu, width, height){

  # uniform with Normal tails
  a <- mu - width / 2
  b <- mu + width / 2
  area <- height * (b - a)
  sig <- (1 - area) / (height * sqrt(2 * pi))
  
  res <- rep(NA, length(x))
  
  for (i in 1:length(res)){
    if ((x[i] >= a) & (x[i] <= b)){res[i] <- height}
    if (x[i] < a){res[i] <- dnorm(x[i], mean = a, sd = sig) * (1 - area)}
    if (x[i] > b){res[i] <- dnorm(x[i], mean = b, sd = sig) * (1 - area)}
  }
  
  return(res)
}