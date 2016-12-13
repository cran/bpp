\name{estimate_toIntegrate}
\alias{estimate_toIntegrate}
\title{Product of posterior density and conditional power for known interim result}
\description{Product of posterior density and conditional power for known interim result, integrate over this function to get BPP.}
\usage{estimate_toIntegrate(x, prior = c("normal", "flat"), successmean, 
            finalsigma, datamean, datasigma, priormean, propA = 0.5, ...)}
\arguments{
  \item{x}{Value at which to evaluate the function.}
  \item{prior}{Prior density on effect sizes.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{finalsigma}{(Known) standard deviation at which the final analysis of the study under consideration takes place.}
  \item{datamean}{Mean of the data.}
  \item{datasigma}{(Known) standard deviation of \code{datamean}.}
  \item{priormean}{Prior mean.}
  \item{propA}{Proportion of subjects randomized to arm A.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp} for examples).}
}
\value{Value of the function, a real number.}
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





