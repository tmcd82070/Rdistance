\name{ESW}
\alias{ESW}

\title{Effective Strip Width for estimated distance functions}

\description{
Computes effective strip width (ESW) for estimated distance functions.
}

\usage{
ESW(obj)
}

\arguments{
  \item{obj}{An estimated distance function object.  An estimated distance 
  function object has class 'dfunc', and is usually produced by a call to 
  \code{F.dfunc.estim}. The estimated distance function may optionally contain 
  a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} = 1
  is assumed.}
}

\details{The effective strip width (ESW) of a distance function is its integral. 
That is, ESW is the area under the 
distance function from its left-truncation limit (\code{obj$w.lo}) to its 
right-truncation limit (\code{obj$w.hi}). 
\if{latex}{In mathematical notation, 
\deqn{ESW = \int_{w.lo}^{w.hi} g(x)dx,}
where \eqn{g(x)} is the height of the distance function at distance \eqn{x}, 
and \eqn{w.lo} and \eqn{w.hi}
are the lower and upper truncation limits used during the survey.  }

The name \emph{effective strip width} derives from the fact that under perfect detection, 
area under the distance function is the half-width of the strip transect.  This means
that if \code{obj$w.lo} = 0 and \eqn{g(x)} = 1,
area under the distance function is the half-width of the transect (i.e., \code{obj$w.hi}).  When 
detection is not perfect, less than the total half-width is \emph{effectively} covered.  
The width that observers \emph{effectively} cover is the ESW.  Buckland et al. 
show why area under the distance function can be interpreted as the ESW.  

The trapazoid rule is used to numerically integrate under the distance function 
in \code{obj} from \code{obj$w.lo} to \code{obj$w.hi}. Two-hundred trapazoids are 
used in the approximation.  In some rare cases, this 
may not be enough.  In these cases, the code for this function can be inspected and modified 
to up this number. 
}

\value{
A scalar equal to the area under the distance function from \code{obj$w.lo} to \code{obj$w.hi}. }

\references{
Buckland et al. 
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}

\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
fit <- F.dfunc.estim(x, w.lo=0, w.hi=100)
ESW(fit)
}

\keyword{ modeling }

