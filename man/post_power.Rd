\name{post_power}
\alias{post_power}
\title{Conditional power conditioning on a blinded interim}
\description{Conditional power conditioning on a blinded interim, i.e. the estimate after the interim is only known to lie in an interval.}
\usage{post_power(x, datasigma, finalsigma, successmean, IntEffBoundary, IntFutBoundary)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{datasigma}{(Known) standard deviation of \code{datamean}.}
  \item{finalsigma}{(Known) standard deviation at which the final analysis of the study under consideration takes place.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis.}
}
\value{Value of the function, a real number.}
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





