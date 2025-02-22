\name{bpp}
\alias{bpp}
\title{Bayesian Predictive Power (BPP) for Normally Distributed Endpoint}
\description{Compute BPP for a Normally distributed endpoint, e.g. log(hazard ratio). Note that this function integrates the power function over the entire real axis, i.e. actually computes assurance as defined in O'Hagan et al. (2001). Bayesian predictive power as defined in Spiegelhalter et al. (1986) only integrates the power function over a range where the effect is considered to be clinically relevant, see Kunzmann et al. (2021) for an extended discussion.}
\usage{bpp(prior = c("normal", "flat"), successmean, finalSE, priormean, ...)}
\arguments{
  \item{prior}{Prior density on effect sizes.}
  \item{successmean}{The mean that defines success at the final analysis. Typically chosen to be the minimal detectable difference, i.e. the critical on the scale of the effect size of interest corresponding to the significance level at the final analysis.}
  \item{finalSE}{(Known) standard error at which the final analysis of the study under consideration takes place.}
  \item{priormean}{Prior mean.}
  \item{...}{Further arguments specific to the chosen prior (see \code{bpp} for examples).}
}
\value{A real number, the bpp.}

\references{
Kunzmann, K., Grayling, M.J., Lee, K.M., Robertson, D.S., Rufibach, K., Wason, J.M.S.,
A Review of Bayesian Perspectives on Sample Size Derivation for Confirmatory Trials.
\emph{Am. Stat.}, \bold{75}(4), 424--432.

O'Hagan, A., Stevens, J. W., Montmartin, J. (2001).
Bayesian cost-effectiveness analysis from clinical trial data.
\emph{Stat. Med.}, \bold{20}(5), 733--753.

Rufibach, K., Jordan, P., Abt, M. (2016a). 
Sequentially Updating the Likelihood of Success of a Phase 3 Pivotal Time-to-Event Trial based on Interim Analyses or External Information. 
\emph{J. Biopharm. Stat.}, \bold{26}(2), 191--201.

Rufibach, K., Burger, H.U., Abt, M. (2016b). 
Bayesian Predictive Power: Choice of Prior and some Recommendations for its Use as Probability of Success in Drug Development.
\emph{Pharm. Stat.}, \bold{15}, 438--446.

Spiegelhalter, D.J., Reedman, L.S., Blackburn, P.R. (1986).
Monitoring clinical trials - conditional power or predictive power.
\emph{Control. Clin. Trials.}, \bold{7}(1), 8--17.
}

\author{
Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@gmail.com}}

\examples{

# type ?bpp_1interim for code of all the computations in Rufibach et al (2016a).

}

\keyword{htest}




