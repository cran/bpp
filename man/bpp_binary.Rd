\name{bpp_binary}
\alias{bpp_binary}
\title{Bayesian Predictive Power (BPP) for Binary Endpoint}
\description{Compute BPP for a binary endpoint.}
\usage{bpp_binary(prior = c("normal", "flat"), successdelta, pi1, n1,
                       pi2, n2, priormean, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successdelta}{The proportion difference that defines success at the final analysis. We assume that higher proportions are better, e.g. as for response in oncology. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{pi1}{Assumed response proportion in intervention arm. Used to compute standard error at final analysis.}
  \item{n1}{Sample size in intervention arm. Used to compute standard error at final analysis.}
  \item{pi2}{Assumed response proportion in control arm. Used to compute standard error at final analysis.}
  \item{n2}{Sample size in control arm. Used to compute standard error at final analysis.}
  \item{priormean}{Prior mean.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_binary} for examples).}
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

# simple example with response proportions
pi1 <- 0.6              # proportion in intervention arm
pi2 <- 0.45             # proportion in control arm
n1 <- 174
n2 <- 174

# MDD at final analysis - proportion difference that corresponds to "success"
mdd <- 0.1

# prior if normal
pi20 <- 0.44
pi10 <- 0.64
n0 <- 50
priormean <- pi10 - pi20
sd0 <- sqrt(pi20 * (1 - pi20) / (n0 / 2) + pi10 * (1 - pi10) / (n0 / 2))

bpp0 <- bpp_binary(prior = "normal", successdelta = mdd, pi1 = pi1, n1 = n1,
           pi2 = pi2, n2 = n2, priormean = priormean, priorsigma = sd0)
bpp0

# prior if flat
width1 <- 0.5
height1 <- 1.5

bpp0_1 <- bpp_binary(prior = "flat", successdelta = mdd, pi1 = pi1, n1 = n1,
                    pi2 = pi2, n2 = n2, priormean = priormean, 
                    width = width1, height = height1)
bpp0_1
}

\keyword{htest}




