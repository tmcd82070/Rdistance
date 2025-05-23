% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneStep.start.limits.R
\name{oneStep.start.limits}
\alias{oneStep.start.limits}
\title{oneStep likelihood start and limit values}
\usage{
oneStep.start.limits(ml)
}
\arguments{
\item{ml}{Either a Rdistance 'model frame' or an Rdistance 
'fitted object'.  Both are of class "dfunc". 
Rdistance 'model frames' are lists containing components 
necessary to estimate a distance function, but no estimates. 
Rdistance 'model frames' are typically
produced by calls to \code{\link{parseModel}}. 
Rdistance 'fitted objects'
are typically produced by calls to \code{\link{dfuncEstim}}.
'Fitted objects' are 'model frames'
with additional components such as the parameters estimates, 
log likelihood value, convergence information, and the variance-
covariance matrix of the parameters.}
}
\value{
A list containing the following components
  \item{start}{Vector of starting values for parameters of the likelihood and expansion terms. }
  \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
  \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
  \item{names}{Vector of names for the likelihood parameters and expansion terms.}
  
The length of each vector in the return is:
  \code{(Num expansions) + 1 + 1*(like \%in\% c("hazrate")) + (Num Covars)}.
}
\description{
Compute starting values and limits 
for the oneStep distance function.
}
\examples{
# make 'model list' object
# Boundary is 10, p is 100 / 120 = 0.833
library(Rdistance)
whi <- 50
x <- c( runif(100, min=0, max=10), runif(20, min=10, max=whi))
x <- units::set_units(x, "m")
detectDf <- data.frame(transect = 1, dist = x)
siteDf <- data.frame(transect = 1, length = units::set_units(10,"m"))
distDf <- RdistDf(siteDf, detectDf)
ml <- parseModel(distDf
            , formula = dist ~ 1
            , w.lo = 0
            , w.hi = units::set_units(whi, "m")
            )
            

sl <- oneStep.start.limits(ml)
hist(x, n = 20)
abline(v = exp(sl$start["(Intercept)"]))


}
\seealso{
[oneStep.like()]
}
