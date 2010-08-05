\name{BootSummary}
\alias{BootSummary}
\concept{bootstrap}

\title{Summarize a Bootstrap Object}
\description{
  Estimates  bias, standard errors, coefficient of variation, and confidence intervals for every estimator in a \code{boot} object.  Confidence intervals are calculated via the \code{boot.ci} function.
}
\usage{
BootSummary(bootObject, CItype = c("bca", "perc"), CONF = 0.95, ROUND = 4)
}
\arguments{
  \item{bootObject}{object created by a call to the \code{\link[boot]{boot}} function}
  \item{CItype}{vector of character strings identifying type of boostrap confidence interval to calculate: \sQuote{bca} - Bias-Corrected accelerated, or \sQuote{perc} - percentile.}
  \item{CONF}{confidence level}
  \item{ROUND}{number of decimal digits to retain when rounding the final values}
}
\details{
\code{BootSummary} calculates a variety of statistical summaries from each bootstrap replicate sampling distribution returned in the component \code{t} of an object created by calling the \code{boot} function. 
}
\value{
  A data frame with the following columns:\cr
  \item{Parameter}{name of parameter estimated from each bootstrap replicate}
  \item{Est}{estimated parameter value from full data set}
  \item{Bias}{bootstrap estimate of bias in the parameter estimation}
  \item{RelativeBias}{bootstrap estimate of relative bias (Bias/Est)}
  \item{StdError}{bootstrap estimate of the standard error for the parameter estimation}
  \item{CV}{bootstrap estimate of the coefficient of variation for the parameter estimation}
  \item{LowerBCa}{Lower \code{(1-CONF)*100\%} confidence interval limit from BCa interval (if requested)}
  \item{UpperBCa}{Upper \code{(1-CONF)*100\%} confidence interval limit from BCa interval (if requested)}
  \item{LowerPercentile}{Lower \code{(1-CONF)*100\%} confidence interval limit from percentile interval (if requested)}
  \item{UpperPercentile}{Upper \code{(1-CONF)*100\%} confidence interval limit from percentile interval (if requested)}
}
\references{\cite{Davison,A.C., and Hinkley, D.V., 1997, Bootstrap Methods and their Application. Cambridge University.}}
\author{Joel H. Reynolds \email{Joel\_reynolds@fws.gov}}

\section{Warning}{
Will abort calculations if any \code{NA}s present in \code{bootObject\$t} (i.e., if any of the replicates fail to converge).

Calculating \sQuote{BCa} intervals may crash if the detection model is difficult to fit, in which case you should likely settle for percentile intervals and rerun.}

\seealso{ \code{\link{FitGMRDSmodel}}, \code{\link{BootstrapGMRDSmodel}}, \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}}}
\examples{
## using Togiak NWR example data, but just 100 replicates to illustrate
## USE MORE REPLICATES IN REAL LIFE
 boot.out <- BootstrapGMRDSmodel(TogiakTransectdf, TogiakObjectdf, 
    FittedModel=TogiakModel,Strata=TogiakTransectdf$YR,    
    UseCubFix=FALSE, R=100)

 ExampleBootSummary<-BootSummary(bootObject=boot.out, CItype=c('perc'),CONF=0.95)

 ExampleBootSummary

## Param.     Est     Bias   RelBias  StdError  CV   LowerPerc'tile  UpperPerc'tile 
##   Ng    378.1178   4.5536  0.0120  44.3266  0.1172  301.2090        472.7558 
##   Na    666.2586   8.5613  0.0128  85.2152  0.1279  524.2119        848.4671
##    ...   
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{nonparametric}

\keyword{univar}

\keyword{survey}% __ONLY ONE__ keyword per line
