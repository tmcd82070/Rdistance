\name{sparrow.covs}
\docType{data}
\alias{sparrow.covs}
\title{Example sparrow distance sampling dataset}
\description{
  This dataset demonstrates the type and format of input data required by \code{Rdistance} to estimate a detection function and abundance from distance sampling data collected by surveying line transects.  Only the data in \code{sparrow.dists} is needed to fit a detection function (see \code{\link{F.dfunc.estim}}), but both \code{sparrow.dists} and \code{sparrow.covs} are needed to estimate abundance (see \code{\link{F.abund.estim}}).  Data come from 72 transects, each 500 meters long, surveyed for sparrows by the Wyoming Cooperative Fish & Wildlife Research Unit.
}

\usage{sparrow.covs}
\format{A data.frame containing 72 rows and 3 columns.  Each row represents a site or transect surveyed.  Columns descriptions:
  \enumerate{
    \item \code{siteID}:  Factor, the site or transect surveyed.
    \item \code{length}:  Number, the length of each transect.  Use the same units for all distance measurements.
    \item \code{sage}:  Factor, an example categorical covariate with two classes.
    \item \code{...}:  Additional transect-level covariate columns.
  }
}


\seealso{\code{\link{sparrow.dists}}}
\source{Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}}
\references{Carlisle and Chalfoun, unpublished data}
\keyword{datasets}