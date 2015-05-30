\name{powerexp.like}
\alias{powerexp.like}
\alias{powerexp.start.limits}

\title{Power exponential likelihood for distance analyses}

\description{
Computes an exponential or normal-like distance function where the power 
in the exponent is an estimated parameter. 
}

\usage{
powerexp.like(a, dist, w.lo = 0, w.hi = max(dist), series = "cosine",
expansions = 0, scale = TRUE)
powerexp.start.limits(dist, expansions, w.lo, w.hi)
}

\arguments{
  \item{a}{Vector of parameter values. First element is \code{sigma}, second is 
   \code{K} (the power), and the remaining are for expansion parameters.  
  }
  \item{dist}{Numeric vector of observed distances.}
  \item{w.lo}{Value of the lowest observable distance.  This is the \emph{left truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values less than \code{w.lo} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{w.hi}{Value of the highest observable distance.  This is the \emph{right truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values greater than \code{w.hi} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{series}{A string specifying the type of expansion to use.  Currently, valid values are 
        'simple', 'hermite', and 'cosine'; but, see \code{\link{F.dfunc.estim}} about defining other series.
  }
  \item{expansions}{A scalar specifying the number of terms in \code{series}. Depending on the series, 
        this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.
  }
  \item{scale}{Logical value indicating whether or not to scale the likelihood to  integrate to 1. This parameter is used to stop recursion in other functions.  If \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) is called, which in turn calls this likelihood function again with \code{scale} = FALSE. Thus, this routine knows when its values are being used to compute the likelihood and when its values are being used to compute the constant of integration.  
  }

}


\details{
    The power exponential likelihood is 
\deqn{f(x|sigma,K) = 1 - exp( - (x/sigma)^{-K} )}{f(x|sigma,K) = 
        1 - exp(-(x/sigma)^(-K))}    

        Large values of K produce uniform-like likelihoods, with support from 
        \code{w.lo} to \code{sigma}.  When K is large, the \code{powerexp} and \code{uniform} (heavy side) likelihoods are similar.
        
        In fact all functions pass through 1 - exp(-1) at \code{sigma}. That is, \code{powerexp( c(s,k), s, scale=FALSE)} equals \code{0.6321} for all \code{K}.  Thus, for large \code{K}, effective strip width is \code{sigma}. 
}

\value{
        A numeric vector the same length and order as \code{dist} containing the likelihood contribution for 
        distances in \code{dist}.  The full log likelihood is  
        \code{-sum(log(powerexp(a,dist)), na.rm=T)}. Note that the returned likelihood value for distances less than \code{w.lo} or 
        greater than \code{w.hi} is \code{NA}.   

        If \code{scale} = TRUE, the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0.  
        If \code{scale} = FALSE, the integral of the likelihood is an arbitrary constant.
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}

\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{halfnorm.like}}
}

\examples{
x <- seq( 0, 100, length=100)

#   Plots showing effects of changes in sigma
plot(x, powerexp.like(c(20,2), x, scale=FALSE), type="l", col="red", ylim=c(0,1))
lines(x, powerexp.like(c(40,2), x, scale=FALSE), col="blue")
lines(x, powerexp.like(c(60,2), x, scale=FALSE), col="green")

#   Plots showing effects of changes in K
plot(x, powerexp.like(c(30,1), x, scale=FALSE), type="l", col="red", ylim=c(0,1))
lines(x, powerexp.like(c(30,2), x, scale=FALSE), col="blue")
lines(x, powerexp.like(c(30,3), x, scale=FALSE), col="green")


#   Fit powerexp likelihood to half-normal data
set.seed(238642)
x <- rnorm(500, 0, 5)
x <- x[ x > 0 ]
dfunc <- F.dfunc.estim( x, likelihood="powerexp" )
plot(dfunc)
}

\keyword{ models }

