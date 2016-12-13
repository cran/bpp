NormalNormalPosterior <- function(datamean, datasigma, n, nu, tau){
     
postmean <- (n / (datasigma ^ 2) + tau ^ (-2)) ^ (-1) * (n * datamean / (datasigma ^ 2) + nu / (tau ^ 2))     
postsigma <- sqrt((n / (datasigma ^ 2) + tau ^ (-2)) ^ (-1))

res <- list("postmean" = postmean, "postsigma" = postsigma)
return(res)
}













#
