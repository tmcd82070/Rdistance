\name{F.start.limits}
\alias{F.start.limits}
\title{
F.start.limits - Set starting values and limits for 
parameters of Rdistance functions.
}
\description{
Return reasonable starting values and limits (boundaries) for the parameters of 
distance functions.  Starting values and limits are specified for 
all likelihoods and expansion terms.  This function is called by 
other routines in \code{Rdistance}, and is not intended to 
be called by the user. 
}

\usage{
F.start.limits(like, expan, w.lo, w.hi, dist)
}

\arguments{
  \item{like}{
    String specifying the likelihood for the distance function.  Possible values are 
    "hazrate" for hazard rate likelihood, "halfnorm" for the half 
    normal likelihood, "uniform" for the uniform likelihood, 
    "negexp" for the negative exponential likelihood, and 
    "Gamma" for the gamma likelihood. 
}
  \item{expan}{
    Number of expansion terms to include. Valid values are 0, 1, ..., 3.
}
  \item{w.lo}{
    Lower or left-truncation limit of the distances.  Normally, 0.
}
  \item{w.hi}{
    Upper or right-truncation limit of the distances. This is the maximum 
    off-transect distance that could be observed. 
}
  \item{dist}{
    The vector of observed off-transect distances being analyzed.  This 
    vector is only required for \code{like} = "Gamma" and "halfnorm". 
}
}

\details{
    The number of parameters to be fitted is 
    \code{expan + 1 + 1*(like \%in\% c("hazrate", "uniform"))}.
    This is the length of all vectors returned in the output list.
}

\value{A list containing the following components
  \item{start}{Vector of reasonable starting values for parameters of the likelihood and 
  expansion terms. }
  \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
  \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
  \item{names}{Vector of names for the likelihood parameters and expansion terms.}
  
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
    F.start.limits( "uniform", 0, 0, 1000 )
    F.start.limits( "uniform", 1, 0, 1000 )
    F.start.limits( "uniform", 2, 0, 1000 )
    F.start.limits( "uniform", 3, 0, 1000 )

    F.start.limits( "halfnorm", 0, 0, 1000, 500*runif(100) )
    F.start.limits( "halfnorm", 1, 0, 1000, 500*runif(100) )
    F.start.limits( "halfnorm", 2, 0, 1000, 500*runif(100) )
    F.start.limits( "halfnorm", 3, 0, 1000, 500*runif(100) )

    F.start.limits( "hazrate", 0, 0, 1000 )
    F.start.limits( "hazrate", 1, 0, 1000 )
    F.start.limits( "hazrate", 2, 0, 1000 )
    F.start.limits( "hazrate", 3, 0, 1000 )

    F.start.limits( "negexp", 0, 0, 1000 )
    F.start.limits( "negexp", 1, 0, 1000 )
    F.start.limits( "negexp", 2, 0, 1000 )
    F.start.limits( "negexp", 3, 0, 1000 )

    F.start.limits( "Gamma", 0, 0, 1000, 1000*runif(100) )
}

\keyword{ models }
