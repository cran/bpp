\name{estimate_posterior}
\alias{estimate_posterior}
\title{Posterior density conditional on known interim result}
\description{If we update the prior with a known estimate at an interim analysis, we get this density.}
\usage{estimate_posterior(x, prior = c("normal", "flat"), datamean, datasigma, priormean, ...)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{prior}{Prior density on effect sizes.}
  \item{datamean}{Mean of the data.}
  \item{datasigma}{(Known) standard deviation of \code{datamean}.}
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





