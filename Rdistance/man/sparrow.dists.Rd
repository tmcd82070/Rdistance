\name{sparrow.dists}
\docType{data}
\alias{sparrow.dists}
\title{Example sparrow distance sampling dataset}
\description{
  This dataset demonstrates the type and format of input data required by \code{Rdistance} to estimate a detection function and abundance from distance sampling data collected by surveying line transects.  Only the data in \code{sparrow.dists} is needed to fit a detection function (see \code{\link{F.dfunc.estim}}), but both \code{sparrow.dists} and \code{sparrow.covs} are needed to estimate abundance (see \code{\link{F.abund.estim}}).  Data come from 72 transects, each 500 meters long, surveyed for sparrows by the Wyoming Cooperative Fish & Wildlife Research Unit.
}

\usage{sparrow.dists}
\format{A data.frame containing 356 rows and 4 columns.  Each row represents a detected group of sparrows.  Columns descriptions:
  \enumerate{
    \item \code{siteID}:  Factor, the site or transect where the detection was made.
    \item \code{groupsize}:  Number, the number of individuals within the detected group.
    \item \code{sightdist}:  Number, the distance from the observer to the detected group.
    \item \code{sightangle}:  Number, the angle (in degrees) from the transect line to the detected group.
  }
}


\seealso{\code{\link{sparrow.covs}}}
\source{Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}}
\references{Carlisle and Chalfoun, unpublished data}
\keyword{datasets}