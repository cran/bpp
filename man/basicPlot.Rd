\name{basicPlot}
\alias{basicPlot}
\title{Basic plot functions to illustrate prior and posterior densities when considering a time-to-event endpoint}
\description{Basic plot function, labels are specific to the hazard ratio, i.e. when looking at a time-to-event endpoint.}
\usage{basicPlot(leg = TRUE, IntEffBoundary = NA, IntFutBoundary = NA, successmean = NA, 
       priormean = NA)}
\arguments{
  \item{leg}{\code{logical}, display legend?}
  \item{IntEffBoundary}{Interim efficacy boundary.}
  \item{IntFutBoundary}{Interim futility boundary.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
    \item{priormean}{Mean of the prior.}
}
\value{Empty generic plot.}

\references{
Rufibach, K., Jordan, P., Abt, M. (2016a). 
Sequentially Updating the Likelihood of Success of a Phase 3 Pivotal Time-to-Event Trial based on Interim Analyses or External Information. 
\emph{J. Biopharm. Stat.}, \bold{26}(2), 191--201.
}

\author{
Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@roche.com}}

\examples{

# type ?bpp_1interim for code of all the computations in Rufibach et al (2016a).

}
\keyword{htest}





