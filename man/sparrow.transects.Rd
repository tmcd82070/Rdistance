\name{sparrow.transects}
\docType{data}
\alias{sparrow.transects}
\title{Example data:  sparrow transects}
\description{
\code{Rdistance} contains two example datasets, i.e., \code{\link{sparrow.detections}} and \code{\link{sparrow.transects}}.  These datasets demonstrate the type and format of input data required by \code{Rdistance} to estimate a detection function and abundance from distance sampling data collected by surveying line transects.  Only the data in \code{sparrow.detections} is needed to fit a detection function (see \code{\link{F.dfunc.estim}}), but both \code{sparrow.detections} and \code{sparrow.transects} are needed to estimate abundance (see \code{\link{F.abund.estim}}).

Data come from 72 transects, each 500 meters long, surveyed for sparrows by the Wyoming Cooperative Fish & Wildlife Research Unit.  Observers recorded the sighting distance and sighting angle to each sparrow group detected.

See the package vignettes for an \code{Rdistance} tutorial using these datasets.
}

\usage{sparrow.transects}
\format{A data.frame containing 72 rows and 5 columns.  Each row represents a site or transect surveyed.  Column descriptions:
  \enumerate{
    \item \code{siteID}:  Factor, the site or transect surveyed.
    \item \code{length}:  Number, the length of each transect.  Use the same units for all distance measurements.
    \item \code{observer}:  Factor, an example categorical covariate with 5 classes.  Identity of the observer who surveyed the transect.
    \item \code{sagemean}:  Number, an example continuous covariate.  The mean sagebrush cover (\%) at each transect.
    \item \code{sage}:  Factor, an example categorical covariate with two classes.  Class is "Low"" when sagemean is < 10\%, "High" otherwise.
  }
}


\seealso{\code{\link{sparrow.detections}}}
\source{Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}}
\references{Carlisle and Chalfoun, unpublished data}
\keyword{datasets}