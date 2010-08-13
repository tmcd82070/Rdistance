\name{coef.dfunc}
\alias{coef.dfunc}

\title{Coefficients of an estimated distance function}

\description{Extract the coefficients and estimated parameters (if any) from 
a estimated distance function object.}

\usage{
\method{coef}{dfunc}(obj, \dots)
}

\arguments{
  \item{obj}{An estimated distance function object.  An estimated distance 
  function object has class 'dfunc', and is usually produced by a call to 
  \code{F.dfunc.estim}. }
  
  \item{\dots}{Required for compatability with the general \code{coef} method.  Any 
  extra arguments to this function are ignored. }
}

\details{
This is an extractor function for the parameters of an estimated distance function. 
This function is equivalent to \code{obj$parameters} for classical distance functions. 
}

\value{
The estimated parameter vector for the distance function. Length and interpretation of values 
in this vector vary depending on the form of the distance function and expansion terms.
}


\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AIC}}, \code{\link{F.dfunc.estim}}
}

\examples{
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
fit <- F.dfunc.estim(x, w.lo=0, w.hi=100)
coef(fit)  
fit <- F.dfunc.estim(x, w.lo=0, w.hi=100, expansions=1)
coef(fit)
}

\keyword{ model }

