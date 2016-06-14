\name{sparrow.detections}
\docType{data}
\alias{sparrow.detections}
\title{Example data:  sparrow detections}

\description{
\code{Rdistance} contains two example datasets, i.e., \code{\link{sparrow.detections}} and \code{\link{sparrow.transects}}.  These datasets demonstrate the type and format of input data required by \code{Rdistance} to estimate a detection function and abundance from distance sampling data collected by surveying line transects.  Only the data in \code{sparrow.detections} is needed to fit a detection function (see \code{\link{F.dfunc.estim}}), but both \code{sparrow.detections} and \code{sparrow.transects} are needed to estimate abundance (see \code{\link{F.abund.estim}}).

Data come from 72 transects, each 500 meters long, surveyed for sparrows by the Wyoming Cooperative Fish & Wildlife Research Unit.  Observers recorded the sighting distance and sighting angle to each sparrow group detected.

See the package vignettes for an \code{Rdistance} tutorial using these datasets.
}

\usage{sparrow.detections}
\format{A data.frame containing 356 rows and 5 columns.  Each row represents a detected group of sparrows.  Column descriptions:
  \enumerate{
    \item \code{siteID}:  Factor, the site or transect where the detection was made.
    \item \code{groupsize}:  Number, the number of individuals within the detected group.
    \item \code{sightdist}:  Number, the distance (in meters) from the observer to the detected group.
    \item \code{sightangle}:  Number, the angle (in degrees) from the transect line to the detected group.
    \item \code{dist}:  Number, the perpendicular, off-transect distance (in meters) from the transect to the detected group.  This is the distance used in analysis.  Calculated using \code{\link{perp.dists}}.
  }
}


\seealso{\code{\link{sparrow.transects}}}
\source{Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}}
\references{Carlisle and Chalfoun, unpublished data}
\keyword{datasets}