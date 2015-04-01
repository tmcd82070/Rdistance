\name{ESW}
\alias{ESW}

\title{Effective Strip Width for estimated detection functions}

\description{
Computes effective strip width (ESW) for estimated detection functions.
}

\usage{
ESW(obj)
}

\arguments{
  \item{obj}{An estimated detection function object.  An estimated detection
  function object has class 'dfunc', and is usually produced by a call to 
  \code{F.dfunc.estim}. The estimated detection function may optionally contain 
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
area under the detection function is the half-width of the strip transect.  This means
that if \code{obj$w.lo} = 0 and \eqn{g(x)} = 1,
area under the detection function is the half-width of the transect (i.e., \code{obj$w.hi}). 
In this case, the density of objects is estimated as number sighted 
divided by area surveyed, which is \code{obj$w.hi} times total length of transects surveyed. 

When detection is not perfect, less than the total half-width is \emph{effectively} covered.  
Buckland et al. (1993)
show that the denominator of the density estimator in this case involves total length of 
transects surveyed times area under the detection function (i.e., this integral). By analogy with the 
perfect detection case, this integral can then be viewed as the 
transect half-width that observers \emph{effectively} cover. In other words, a survey with imperfect detection
and ESW equal to X effectively covers the same area as a study with perfect detection out to a distance of X.

The trapazoid rule is used to numerically integrate under the distance function 
in \code{obj} from \code{obj$w.lo} to \code{obj$w.hi}. Two-hundred trapazoids are 
used in the approximation to speed calculations.  In some rare cases, two hundred trapazoids 
may not be enough.  In these cases, the code for this function can be \code{sink}-ed to a file, 
inspected in a text editor, modified 
to bump the number of trapazoids, and \code{source}-d back in. 
}

\value{
A scalar equal to the area under the detection function from \code{obj$w.lo} to \code{obj$w.hi}. }

\references{
Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L. 1993. \emph{Distance Sampling: Estimating Abundance of Biological Populations}. Chapman and Hall, London.
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}

\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
# Load the example sparrows distances dataset from package
data(sparrow.dists)

# Compute perpendicular, off-transect distances from the observer's sight distance and angle
sparrow.dists$dists <- perp.dists(obs.dist=sparrow.dists$sightdist,
                                  obs.angle=sparrow.dists$sightangle)

# Fit detection function object
dfunc <- F.dfunc.estim(sparrow.dists, w.hi=150)

# Compute effective strip width (ESW)
ESW(dfunc)
}

\keyword{ modeling }

