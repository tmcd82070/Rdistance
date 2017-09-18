#' Brewer's Sparrow Detection Data (line-transect survey)
#' 
#' \code{Rdistance} contains four example datasets: two collected using a
#' line-transect survey (i.e., \code{\link{sparrowDetectionData}} and
#' \code{\link{sparrowSiteData}}) and two collected using a point-transect
#' (sometimes called a point count) survey (i.e.,
#' \code{\link{thrasherDetectionData}} and \code{\link{thrasherSiteData}}).
#'   These datasets demonstrate the type and format of input data required by
#' \code{Rdistance} to estimate a detection function and abundance from
#' distance sampling data collected by surveying line transects or point
#' transects.  They also allow the user to step through the tutorials described
#' in the package vignettes.  Only the detection data is needed to fit a
#' detection function (if there are no covariates in the detection function;
#' see \code{\link{dfuncEstim}}), but both detection and
#' the additional site data are needed to estimate abundance (or to include
#' site-level covariates in the detection function; see
#' \code{\link{abundEstim}}).
#' 
#' Line transect (sparrow) data come from 72 transects, each 500 meters long,
#' surveyed for Brewer's Sparrows by the Wyoming Cooperative Fish & Wildlife
#' Research Unit in 2012.
#' 
#' Point transect (thrasher) data come from 120 points surveyed for Sage
#' Thrashers by the Wyoming Cooperative Fish & Wildlife Research Unit in 2013.
#' 
#' See the package vignettes for \code{Rdistance} tutorials using these
#' datasets.
#' 
#' 
#' @name sparrowDetectionData
#' 
#' @docType data
#' 
#' @format A data.frame containing 356 rows and 5 columns.  Each row represents
#' a detected group of sparrows.  Column descriptions: \enumerate{ \item
#' \code{siteID}: Factor (72 levels), the site or transect where the detection
#' was made.  \item \code{groupsize}: Number, the number of individuals within
#' the detected group.  \item \code{sightdist}: Number, the distance (m) from
#' the observer to the detected group.  \item \code{sightangle}: Number, the
#' angle (degrees) from the transect line to the detected group.  \item
#' \code{dist}: Number, the perpendicular, off-transect distance (m) from the
#' transect to the detected group.  This is the distance used in analysis.
#' Calculated using \code{\link{perpDists}}.  }
#' 
#' @seealso \code{\link{sparrowSiteData}}
#' 
#' @source A subset of Jason Carlisle's dissertation data, University of Wyoming.
#' 
#' @references Carlisle, J.D. 2017. The effect of sage-grouse conservation on
#' wildlife species of concern: implications for the umbrella species concept.
#' Dissertation. University of Wyoming, Laramie, Wyoming, USA.

#' @keywords datasets
NULL