\name{bpp_continuous}
\alias{bpp_continuous}
\title{Bayesian Predictive Power (BPP) for Continuous Endpoint}
\description{Compute BPP for a continuous endpoint.}
\usage{bpp_continuous(prior = c("normal", "flat"), successmean, stDev, 
                      n1, n2, priormean, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successmean}{The mean difference that defines success at the final analysis. We assume that a higher mean is better. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{stDev}{Standard deviation of measurements in one group. Used to compute standard error at final analysis.}
  \item{n1}{Sample size in intervention arm. Used to compute standard error at final analysis.}
  \item{n2}{Sample size in control arm. Used to compute standard error at final analysis.}
  \item{priormean}{Prior mean.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_continuous} for examples).}
}
\value{A real number, the bpp.}

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

# sample size at final analysis
n1 <- 92
n2 <- 92

# MDD at final analysis (corresponds to delta = 10 for 80% power)
mdd <- 7.023506

# prior
priormean <- 12.3

# standard error for prior, based on Phase 2 data
sig1 <- 26.1
n1p <- 25
sig2 <- 33.6
n2p <- 25
sd0 <- sqrt(sig1 ^ 2 / n1p + sig2 ^ 2 / n2p)

# flat prior
width1 <- 25
height1 <- 0.02

# bpps
bpp_continuous(prior = "normal", successmean = mdd, stDev = stDev, 
                        n1 = n1, n2 = n2, priormean = priormean, priorsigma = sd0)
bpp_continuous(prior = "flat", successmean = mdd, stDev = stDev, 
                          n1 = n1, n2 = n2, priormean = priormean, 
                          width = width1, height = height1)

}

\keyword{htest}




