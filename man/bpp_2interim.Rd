\name{bpp_2interim}
\alias{bpp_2interim}
\title{Bayesian Predictive Power (BPP) for Normally Distributed Endpoint}
\description{Compute BPP and posterior density for a Normally distributed endpoint, e.g. log(hazard ratio), assuming the trial did not stop at two blinded interim analyses.}
\usage{bpp_2interim(prior = "normal", datasigma, finalsigma, successmean, IntEffBoundary, 
                    IntFutBoundary, priormean, thetas, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes. So far, this function only accomodates a Normal prior, as opposed to \code{bpp_1interim} where also the pessimistic prior introduced in Rufibach et al (2016a) can be specified.}
  \item{datasigma}{(Known) standard error of estimate at interim analysis.}
  \item{finalsigma}{(Known) standard error at which the final analysis of the study under consideration takes place.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{IntEffBoundary}{2-d vector of efficacy boundaries at the interim analyses.}
  \item{IntFutBoundary}{2-d vector of futility boundary at the interim analyses.}
  \item{priormean}{Prior mean.}
  \item{thetas}{Grid to compute posterior density on.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp_1interim} for examples).}
}
\value{A list containing the following elements:
\item{initial BPP}{BPP based on the prior.}
\item{BPP after not stopping at interim interval}{BPP after not stopping at a blinded interim.}
\item{posterior density interval}{The posterior density, interval knowledge.}
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
\donttest{ 
# ------------------------------------------------------------------------------------------
# Illustrate the update after two passed interims using the Gallium clinical trial
# ------------------------------------------------------------------------------------------

# ------------------------------------------
# set all parameters:
# ------------------------------------------
# prior mean / sd
hr0 <- 0.9288563
priormean <- log(hr0)
priorsigma <- sqrt(4 / 12)

# specifications for pivotal study
propA <- 0.5   # proportion of patients randomized to arm A
fac <- (propA * (1 - propA)) ^ (-1)
nevents <- c(111, 248, 370)
datasigma <- sqrt(fac / nevents[1:2])
finalsigma <- sqrt(fac / nevents[3])
za <- c(3.9285726330559, 2.5028231888636, 1.9936294555664)
alphas <- 2 * (1 - pnorm(za))
hrMDD <- exp(- za * sqrt(fac / nevents))
successmean <- log(hrMDD[3])

# efficacy and futility interim boundary
effi <- log(c(0, hrMDD[2]))
futi <- log(c(1, Inf))

# grid to compute densities on
thetas <- seq(-0.65, 0.3, by = 0.01)

bpp_2interim(prior = "normal", datasigma = datasigma, finalsigma = finalsigma, 
             successmean = successmean, IntEffBoundary = effi, IntFutBoundary = futi, 
             priormean = priormean, thetas = thetas, priorsigma = priorsigma)
}}

\keyword{htest}





