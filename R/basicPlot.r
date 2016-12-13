basicPlot <- function(leg = TRUE, IntEffBoundary = NA, IntFutBoundary = NA, successmean = NA, priormean = NA){
     
     legcols <- c("coral", "brown", "turquoise1", "blue", "purple4")
     abline(h = 0, lty = 3)
     xlabs <- seq(0, 5, by = 0.1)
     axis(1, at = log(xlabs), labels = xlabs, line = 4.5)
     mtext("true hazard ratio", side = 1, line = 6.5)
     mtext(expression(theta*" = log(true hazard ratio)"), side = 1, line = 2.2)
     if (leg == TRUE){
          legend("topleft", c("prior", "efficacy boundary", "success", "futility boundary"), lty = 1, 
                 lwd = 2, title = "hazard ratio:", bty = "n", title.adj = 0.1, col = legcols)}
     
     segments(priormean, 0, priormean, 20, col = legcols[1], lty = 2, lwd = 3)
     segments(successmean, 0, successmean, 20, col = legcols[3], lty = 3, lwd = 3)
     
     int <- c(IntEffBoundary, IntFutBoundary)
     segments(int, c(-0.12, -0.03), int, c(-0.03, -0.15), lwd = 1)
     segments(int[1], -0.075, int[2], -0.075, lwd = 1)
     segments(int[1], 0, int[1], 10, lty = 2, col = grey(0.5))
     segments(int[2], 0, int[2], 10, lty = 2, col = grey(0.5))
}
