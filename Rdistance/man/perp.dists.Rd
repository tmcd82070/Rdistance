\name{perp.dists}
\alias{perp.dists}

\title{Compute off-transect distances from sighting distance and angle}

\description{
Computes off-transect (also called 'perpendicular') distances from measures of sighting distance and sighting angle.
}

\usage{
perp.dists(obs.dist, obs.angle, digits=1)
}


\arguments{
  \item{obs.dist}{A column of a data.frame that contains the observed or sighting distances from the observer to the
  detected objects.}
  
  \item{obs.angle}{A column of a data.fame that contains the observed or sighting angles from the line transect to the
  detected objects.  Angles must be measured in degrees.}

  \item{digits}{Scalar number of digits to round output distances to.}


}

\details{
  If observers recorded sighting distance and sighting angle (as is often common in line transect surveys), use this function to convert to off-transect distances, the required input data for \code{F.dfunc.estim}.

}

\value{
A vector of off-transect (or perpendicular) distances.  Units are the same as \code{obs.dist}.
}

\references{
Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L. 1993. \emph{Distance Sampling: Estimating Abundance of Biological Populations}. Chapman and Hall, London.
}

\author{
Jason Carlisle,  \email{jason.d.carlisle@gmail.com}
}


\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
# Load the example sparrows dataset from package
data(sparrows)

# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
dists.df$dists <- perp.dists(obs.dist=dists.df$sightdist, obs.angle=dists.df$sightangle, digits=1)

# Proceed to fitting the detection function with F.dfunc.estim
}


