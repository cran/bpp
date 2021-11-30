\name{bpp_1interim_continuous}
\alias{bpp_1interim_continuous}
\title{Bayesian Predictive Power (BPP) for Continuous Endpoint}
\description{Compute BPP and posterior density for a continuous endpoint, e.g. mean difference, assuming either an unblinded or blinded interim result.}
\usage{bpp_1interim_continuous(prior = c("normal", "flat"), successmean, stDev, 
                                    n1, n2, IntEffBoundary, IntFutBoundary, IntFix, 
                                    priormean, propA = 0.5, thetas, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successmean}{The mean difference that defines success at the final analysis. We assume that a higher mean is better. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{stDev}{Standard deviation of measurements in one group. Used to compute standard error at final analysis.}
  \item{n1}{2-d vector of sample sizes in intervention arm, at interim and final analysis.}
  \item{n2}{2-d vector of sample sizes in control arm, at interim and final analysis.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis.}
  \item{IntFix}{Effect sizes observed at the interim analyis, to compute BPP for an unblinded interim analysis.}
  \item{priormean}{Prior mean.}
  \item{propA}{Proportion of subjects randomized to arm A.}
  \item{thetas}{Grid to compute posterior density on.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_1interim_continuous} for examples).}
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

# standard deviation of measurments in one group
stDev <- 24

# number of patients at interim
n1_int <- 46
n2_int <- 46

# number of patients at final analysis
n1 <- 92
n2 <- 92

# MDD at final analysis (corresponds to delta = 10 for 80% power)
mdd <- 7.023506

# efficacy boundary
effi <- 15

# futility boundary --> chosen informally
futi <- 0

# prior
priormean <- 12.3
sd0 <- 4.2

# flat prior
width1 <- 25
height1 <- 0.02

thetas <- seq(-0.65, 0.3, by = 0.01)

bpp_1interim_continuous(prior = "normal", successmean = mdd, stDev = stDev, 
                                 n1 = c(n1_int, n1), n2 = c(n2_int, n2), 
                                 IntEffBoundary = effi, IntFutBoundary = futi, IntFix = 1, 
                                 priormean = priormean, propA = 0.5, thetas, 
                                 priorsigma = sd0)[[1]]
bpp_1interim_continuous(prior = "flat", successmean = mdd, stDev = stDev, 
                                   n1 = c(n1_int, n1), n2 = c(n2_int, n2), IntEffBoundary = effi, 
                                   IntFutBoundary = futi, IntFix = 1, 
                                   priormean = 12.3, propA = 0.5, thetas = thetas, 
                                   width = width1, height = height1)[[1]]
}

\keyword{htest}





