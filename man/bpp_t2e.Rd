\name{bpp_t2e}
\alias{bpp_t2e}
\title{Bayesian Predictive Power (BPP) for Time-To-Event Endpoint}
\description{Compute BPP for a time-to-event endpoint.}
\usage{bpp_t2e(prior = c("normal", "flat"), successHR, d, propA = 0.5,
                    priorHR, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successHR}{The hazard ratio that defines success at the final analysis. We assume that a hazard ratio below 1 corresponds to better outcome. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{d}{Number of events at final analysis.}
  \item{propA}{Proportion of subjects randomized to arm A.}
  \item{priorHR}{Hazard ratio around which the prior is centered.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_t2e} for examples).}
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

# hazard ratio to beat at final analysis
hrMDD <- 0.8173

# number of events at final analysis
nevents <- 381

# prior
hr0 <- 0.7

# SE for a normal prior corresponding to information of 50 events in 1:1 randomized trial
sd0 <- sqrt(4 / 50)

# parameters of flat prior
width1 <- 0.5
height1 <- 1

# compute bpp
bpp_t2e(prior = "normal", successHR = hrMDD, d = nevents,
                 priorHR = hr0, priorsigma = sd0)
bpp_t2e(prior = "flat", successHR = hrMDD, d = nevents,
                   priorHR = hr0, width = width1, height = height1)

}

\keyword{htest}




