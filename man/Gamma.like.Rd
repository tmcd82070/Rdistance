\name{Gamma.like}
\alias{Gamma.like}

\title{Gamma likelihood for distance analyses}

\description{
Computes the gamma likelihood, scaled appropriately, for use as a likelihood in estimating a distance function. 
}

\usage{
Gamma.like(a, dist, w.lo = 0, w.hi = max(dist), series = "cosine",
expansions = 0, scale = TRUE)
}

\arguments{
  \item{a}{Vector of gamma parameter values. At present, \code{a} has length 2, where \code{a[1]} = r =
        shape parameter of the gamma, and \code{a[2]} = lambda = multiplier in shape parameter of the gamma. 
        Support for the gamma distribution, as parameterized here, is (r > 1) and (lambda >= 0). 
        No series or expansions are currently implemented for the gamma likelihood. 
  }
  \item{dist}{Numeric vector of observed distances.}
  \item{w.lo}{Scalar value of the lowest observable distance.  This is the \emph{left truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values less than \code{w.lo} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{w.hi}{Scalar value of the highest observable distance.  This is the \emph{right truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values greater than \code{w.hi} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{series}{Included for compatibility with \code{F.dfunc.estim}. Currently, no series expansions are 
        implemented for the gamma likelihood. 
  }
  \item{expansions}{Included for compatibility with \code{F.dfunc.estim}. Currently, no series expansions are 
        implemented for the gamma likelihood. 
  }
  \item{scale}{Logical scalar indicating whether or not to scale the likelihood to 
        integrate to 1. This parameter is used to stop recursion in the computations.  If 
        \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) 
        is called, which in turn calls this likelihood function again with \code{scale} = FALSE. Thus, this 
        routine knows when its value is being used to compute the likelihood and when its value is 
        being used to compute the constant of integration.  All user defined likelihoods must 
        have and use this parameter.}
}


\details{
    This function utilizes the built-in R function \code{dgamma} to evaluate the gamma 
    density function.  Using the parameterization of \code{dgamma}, the gamma shape parameter 
    is \code{a[1]} while the gamma scale parameter is \code{(a[2]/gamma(r)) * (((r - 1)/exp(1))^(r - 1))}.
    
    Currently, this function implements is a non-covariate version of the gamma distance function 
    used by Becker and Quan (\emph{citation?}).  In future, linear equations will relate covariate values to 
    values of the gamma paramters.  This future implementation will fully replicate the distance 
    functions of Becker and Quan. 
}

\value{
        A numeric vector the same length and order as \code{dist} containing the likelihood contribution for 
        distances in \code{dist}.  Assuming \code{L=gamma.like(c(r,lam),dist)}, the full log likelihood of all the data is  
        \code{-sum(log(L), na.rm=T)}. Note that the returned likelihood value for distances less than \code{w.lo} or 
        greater than \code{w.hi} is \code{NA}, and thus it is prudent to use \code{na.rm=TRUE}
        in the sum.   

        If \code{scale} = TRUE, the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0.  
        If \code{scale} = FALSE, the integral of the likelihood is an arbitrary constant.
}
\references{
Becker and Quan (?)
}
\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}

\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{halfnorm.like}}, \code{\link{hazrate.like}}, 
\code{\link{uniform.like}}, \code{\link{negexp.like}}
}

\examples{
\dontrun{
set.seed(238642)
x <- seq( 0, 100, length=100)

#   Plots showing effects of changes in shape
plot(x, Gamma.like(c(20,20), x), type="l", col="red")
lines(x, Gamma.like(c(40,20), x), type="l", col="blue")

#   Plots showing effects of changes in scale
plot(x, Gamma.like(c(20,20), x), type="l", col="red")
lines(x, Gamma.like(c(20,40), x), type="l", col="blue")


#   Estimate 'Gamma' distance function
r <- 5
lam <- 10
b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
x <- rgamma(1000, shape=r, scale=b*lam)
dfunc <- F.dfunc.estim( x, likelihood="Gamma" )
plot(dfunc)
}
}

\keyword{ models }

