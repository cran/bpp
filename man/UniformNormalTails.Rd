\name{UniformNormalTails}
\alias{dUniformNormalTails}
\alias{pUniformNormalTails}
\title{Density and CDF for Uniform Distribution with Normal tails}
\description{Density function and cumulative distribution function for a Uniform density with Normal tails. Introduced in Rufibach et al (2016a) as pessimistic distribution to compute Bayesian Predictive Power.}
\usage{
dUniformNormalTails(x, mu, width, height)
pUniformNormalTails(x, mu, width, height)
}
\arguments{
  \item{x}{Vector of quantiles.}
  \item{mu}{Mean of the pessimistic prior.}
  \item{width}{Width of the flat part of the prior.}
  \item{height}{Height of the flat part of the prior.}
}
\value{Density at \code{x}.}
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





