\name{interval_posterior_nominator2}
\alias{interval_posterior_nominator2}
\title{Posterior density conditional on two interim results, both only known as intervals, is proportional to the value of this function}
\description{If we update the prior with the knowledge that two interim estimates were between a futility and efficacy boundary, we get a density that is proportional to the value of this function.
}
\usage{interval_posterior_nominator2(x, prior = "normal", 
          IntEffBoundary, IntFutBoundary, datasigma, priormean, ...)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{prior}{Prior density on effect sizes.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis.}
  \item{datasigma}{(Known) standard deviation of \code{datamean}, i.e. at interim analysis.}
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

# type ?bpp_2interim for code of all the computations in Rufibach et al (2016a).

}

\keyword{htest}





