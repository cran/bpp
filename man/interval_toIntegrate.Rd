\name{interval_toIntegrate}
\alias{interval_toIntegrate}
\title{Product of posterior density and conditional power for blinded interim result}
\description{Product of posterior density and conditional power for blinded interim result, integrate over this function to get BPP.}
\usage{interval_toIntegrate(x, prior = c("normal", "flat"), interimSE, 
             finalSE, successmean, IntEffBoundary, IntFutBoundary, 
             priormean, ...)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{prior}{Prior density on effect sizes.}
  \item{interimSE}{(Known) standard error of \code{interimmean}, i.e. at interim analysis.}
  \item{finalSE}{(Known) standard error at which the final analysis of the study under consideration takes place.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis.}
  \item{priormean}{Prior mean.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp} for examples).}
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





