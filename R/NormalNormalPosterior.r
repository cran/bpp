NormalNormalPosterior <- function(datamean, datasigma, n, nu, tau){
     
SE <- sqrt(datasigma ^ 2 / n)
postmean <- (n / (SE ^ 2) + tau ^ (-2)) ^ (-1) * (datamean / (SE ^ 2) + nu / (tau ^ 2))     
postsigma <- sqrt((1 / (SE ^ 2) + tau ^ (-2)) ^ (-1))

res <- list("postmean" = postmean, "postsigma" = postsigma)
return(res)
}













#
