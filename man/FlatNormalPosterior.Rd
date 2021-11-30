\name{FlatNormalPosterior}
\alias{FlatNormalPosterior}
\title{Integrand to compute Bayesian Predictive Power when flat prior has been updated with likelihood}
\description{Assume we have a flat prior on our effect, update it with a Normal likelihood and then want to compute Bayesian Predictive Power. This function provides the integrand for that computation, i.e. the product of the power function and the posterior.}
\usage{FlatNormalPosterior(x, successmean, finalSE, interimmean, interimSE, priormean, 
                           width, height)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{finalSE}{(Known) standard error at which the final analysis of the study under consideration takes place.}
  \item{interimmean}{Mean of the data.}
  \item{interimSE}{(Known) standard error of \code{interimmean}.}
  \item{priormean}{Prior mean.}
  \item{width}{Width of the flat part of the prior.}
  \item{height}{Height of the flat part of the prior.}
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





