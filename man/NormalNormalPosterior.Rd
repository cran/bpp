\name{NormalNormalPosterior}
\alias{NormalNormalPosterior}
\title{Normal-Normal Posterior in conjugate normal model, for known sigma}
\description{Compute the posterior distribution in a conjugate normal model for known variance: Let \eqn{X_1, \ldots, X_n} be a sample from a \eqn{N(\mu, \sigma^2)} distribution, with \eqn{\sigma} assumed known. We assume a prior distribution on \eqn{\mu}, namely \eqn{N(\nu, \tau^2)}. The posterior distribution is then \eqn{\mu|x \sim N(\mu_p, \sigma_p^2)} with

\deqn{\mu_p = (1 / (\sigma^2 / n) + \tau^{-2})^{-1}  (\bar{x} / (\sigma^2/n) + \nu / \tau^2)}

and 

\deqn{\sigma_p = (1 / (\sigma^2/n) + \tau^{-2})^{-1}.}

These formulas are available e.g. in Held (2008, p. 147ff).
}
\usage{NormalNormalPosterior(datamean, datasigma, n, nu, tau)}
\arguments{
  \item{datamean}{Mean of the data.}
  \item{datasigma}{(Known) standard deviation of \code{datamean}.}
  \item{n}{Number of observations.}
  \item{nu}{Prior mean.}
  \item{tau}{Prior standard deviation.}
}
\value{
A list with the entries:
  \item{postmean}{Posterior mean.}
  \item{postsigma}{Posterior standard deviation.}
}
\references{
Held, L., Sabanes-Bove, D. (2014). 
\emph{Applied Statistical Inference}.
Springer.
}

\author{
Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@roche.com}}

\examples{
## data:
n <- 25
sd0 <- 3
x <- rnorm(n, mean = 2, sd = sd0)

## prior:
nu <- 0
tau <- 2

## posterior:
NormalNormalPosterior(datamean = mean(x), datasigma = sd0, n = 77, nu = nu, tau = tau)
}

\keyword{htest}





