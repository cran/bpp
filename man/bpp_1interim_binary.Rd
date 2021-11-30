\name{bpp_1interim_binary}
\alias{bpp_1interim_binary}
\title{Bayesian Predictive Power (BPP) for Binary Endpoint}
\description{Compute BPP and posterior density for a binary endpoint, e.g. response proportions, assuming either an unblinded or blinded interim result.}
\usage{bpp_1interim_binary(prior = c("normal", "flat"), successdelta, pi1, n1,
                                pi2, n2, IntEffBoundary, IntFutBoundary, 
                                IntFix, priormean, propA = 0.5, thetas, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successdelta}{The proportion difference that defines success at the final analysis. We assume that higher proportions are better, e.g. as for response in oncology. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{pi1}{Assumed response proportion in intervention arm.}
  \item{n1}{2-d vector of sample sizes in intervention arm, at interim and final analysis.}
  \item{pi2}{Assumed response proportion in control arm.}
  \item{n2}{2-d vector of sample sizes in control arm, at interim and final analysis.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis.}
  \item{IntFix}{Effect sizes observed at the interim analyis, to compute BPP for an unblinded interim analysis.}
  \item{priormean}{Prior mean.}
  \item{propA}{Proportion of subjects randomized to arm A.}
  \item{thetas}{Grid to compute posterior density on.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_1interim_binary} for examples).}
}
\value{A list containing the following elements:
\item{initial BPP}{BPP based on the prior.}
\item{BPP after not stopping at interim interval}{BPP after not stopping at a blinded interim, provides the results corresponding to \code{IntEffBoundary} and \code{IntFutBoundary}.}
\item{BPP after not stopping at interim exact}{BPP after not stopping at an unblinded interim, provides the results corresponding to \code{IntFix}.}
\item{posterior density interval}{The posterior density, interval knowledge, i.e. corresponding to \code{IntEffBoundary} and \code{IntFutBoundary}.}
\item{posterior power interval}{The posterior power, interval knowledge, i.e. corresponding to \code{IntEffBoundary} and \code{IntFutBoundary}.}
\item{posterior density exact}{The posterior density, exact knowledge of interim result, i.e. corresponding to \code{IntFix}.}
}

\references{
Rufibach, K., Jordan, P., Abt, M. (2016a). 
Sequentially Updating the Likelihood of Success of a Phase 3 Pivotal Time-to-Event Trial based on Interim Analyses or External Information. 
\emph{J. Biopharm. Stat.}, \bold{26}(2), 191--201.

Rufibach, K., Burger, H.U., Abt, M. (2016b). 
Bayesian Predictive Power: Choice of Prior and some Recommendations for its Use as Probability of Success in Drug Development.
\emph{Pharm. Stat.}, \bold{15}, 438--446.
}

\author{
Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@roche.com}}

\examples{

# simple example with response proportions
pi1 <- 0.6              # proportion in intervention arm
pi2 <- 0.45             # proportion in control arm
n1 <- 174
n2 <- 174

# MDD at final analysis - proportion difference that corresponds to "success"
mdd <- 0.1

n1_int <- 87
n2_int <- 87

# prior if normal
pi20 <- 0.44
pi10 <- 0.64
n0 <- 50
priormean <- pi10 - pi20
sd0 <- sqrt(pi20 * (1 - pi20) / (n0 / 2) + pi10 * (1 - pi10) / (n0 / 2))

# efficacy boundary
effi <- 0.2

# futility boundary
futi <- 0

thetas <- seq(-0.65, 0.3, by = 0.01)

bpp_1interim_binary(prior = "normal", successdelta = mdd, pi1 = pi1, 
                    n1 = c(n1_int, n1), n2 = c(n2_int, n2), 
                    pi2 = pi2, IntEffBoundary = effi, IntFutBoundary = futi, 
                    IntFix = 1, priormean = priormean, propA = 0.5, 
                    thetas = thetas, priorsigma = sd0)[[1]]
                             
}

\keyword{htest}





