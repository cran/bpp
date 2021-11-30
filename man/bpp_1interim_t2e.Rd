\name{bpp_1interim_t2e}
\alias{bpp_1interim_t2e}
\title{Bayesian Predictive Power (BPP) for Time-to-Event Endpoint}
\description{Compute BPP and posterior density for a time-to-event endpoint, e.g. hazard ratio, assuming either an unblinded or blinded interim result.}
\usage{bpp_1interim_t2e(prior = c("normal", "flat"), successHR, d,
                             IntEffBoundary, IntFutBoundary, IntFixHR, 
                             priorHR, propA = 0.5, thetas, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successHR}{The hazard ratio that defines success at the final analysis. We assume that hazard ratios below 1 are beneficial. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{d}{2-d vector with number of events at interim and final analysis. Used to compute standard errors.}
  \item{IntEffBoundary}{Efficacy boundary at the interim analysis, hazard ratio.}
  \item{IntFutBoundary}{Futility boundary at the interim analysis, hazard ratio.}
  \item{IntFixHR}{Effect sizes observed at the interim analyis, to compute BPP for an unblinded interim analysis.}
  \item{priorHR}{Hazard ratio at which prior is centered.}
  \item{propA}{Proportion of subjects randomized to arm A.}
  \item{thetas}{Grid to compute posterior density on.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_1interim_t2e} for examples).}
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

# number of events 
nevents <- c(191, 381)

# MDD at final analysis
hrMDD <-  0.8172823

# efficacy boundary
hrEffi <- 0.6508829

# futility boundary --> chosen informally
hrFuti <- 1

# prior specifications

# Normal prior corresponding to information of 50 events in 1:1 randomized trial
hr0 <- 0.7
sd0 <- sqrt(4 / 50)

# flat prior
width1 <- 0.5
height1 <- 1

# compute bpps
thetas <- seq(0.5, 1.35, by = 0.01)
bpp1b <- bpp_1interim_t2e(prior = "normal", successHR = hrMDD, d = nevents,
                          IntEffBoundary = hrEffi, IntFutBoundary = hrFuti, 
                          IntFixHR = 1, priorHR = hr0, propA = 0.5, thetas = thetas, 
                          priorsigma = sd0)[[1]]
bpp1_1b <- bpp_1interim_t2e(prior = "flat", successHR = hrMDD, d = nevents, 
                            IntEffBoundary = hrEffi, IntFutBoundary = hrFuti, 
                            IntFixHR = 1, priorHR = hr0, propA = 0.5, thetas = thetas, 
                            width = width1, height = height1)[[1]]
}

\keyword{htest}





