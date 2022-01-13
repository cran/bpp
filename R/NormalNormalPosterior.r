NormalNormalPosterior <- function(datamean, sigma, n, nu, tau){
     
postmean <- (n / (sigma ^ 2) + tau ^ (-2)) ^ (-1) * (n * datamean / (sigma ^ 2) + nu / (tau ^ 2))     
postsigma <- sqrt((n / (sigma ^ 2) + tau ^ (-2)) ^ (-1))

res <- list("postmean" = postmean, "postsigma" = postsigma)
return(res)
}













#
