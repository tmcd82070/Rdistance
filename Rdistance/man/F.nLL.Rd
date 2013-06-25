\name{F.nLL}
\alias{F.nLL}
\title{
F.nLL - Return the negative log likelihood for a set of distance
values.}

\description{
Return value of the negative log likelihood for a set of observed 
off-transect distances given a specified likelihood, 
number of expansion terms, and estimated parameters. 
}

\usage{
F.nLL(a, dist, like, w.lo = 0, w.hi = max(dist), series, expansions = 0)
}

\arguments{
  \item{a}{A vector of parameter values to use to compute the liklihood. Length of this vector 
    must be \code{expansions + 1 + 1*(like \%in\% c("hazrate", "uniform"))}.}
  \item{dist}{ A vector of observed off-transect distances. Must be between \code{w.lo} and 
    \code{w.hi} (see below).}
  \item{like}{String specifying the form of the likelihood.  Valid values at present are "uniform", 
    "halfnorm", "hazrate", "negexp", and "Gamma". }
  \item{w.lo}{Lower or left-truncation limit of the distances.  This is the minimum 
    possible off-transect distance. Default is 0.}
  \item{w.hi}{Upper or right-truncation limit of the distances. This is the maximum 
    off-transect distance that could be observed. Default is the maximum observed
    distance.}  
\item{series}{String specifying the type of expansion to use series if \code{expansions} > 0.
    Valid values at present are 'simple', 'hermite', and 'cosine'. }
  \item{expansions}{A scalar specifying the number of terms in \code{series} to compute. Depending on the series, 
        this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.}
}

\value{A scalar, the negative of the log likelihood given the parameters in \code{a}, likelihood, and 
    expansion terms.
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}



\seealso{
See \code{\link{uniform.like}} for definitions of all the likelihoods; \code{\link{F.dfunc.estim}}
}

\examples{
set.seed(37372)
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
F.nLL( c(50,20), x, "uniform", w.hi = 100)
F.nLL( c(50,20, .1), x, "uniform", expansions=1, w.hi = 100, series="cosine")

}
\keyword{ model }

