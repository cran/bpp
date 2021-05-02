\name{bpp}
\alias{bpp}
\title{Bayesian Predictive Power (BPP) for Normally Distributed Endpoint}
\description{Compute BPP for a Normally distributed endpoint, e.g. log(hazard ratio).}
\usage{bpp(prior = c("normal", "flat"), direction_lower = TRUE, 
           successmean, finalsigma, priormean, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{direction_lower}{\code{logical}. If \code{TRUE} consider probability \eqn{P(\hat \delta_{fin} \le \delta_{suc})}, applies e.g. for hazard ratio.}  
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{finalsigma}{(Known) standard deviation at which the final analysis of the study under consideration takes place.}
  \item{priormean}{Prior mean.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp} for examples).}
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

# type ?bpp_1interim for code of all the computations in Rufibach et al (2016a).

}

\keyword{htest}




