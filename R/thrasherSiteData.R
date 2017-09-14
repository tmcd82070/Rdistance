#' Sage Thrasher Site Data (point-transect survey)
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
#' see \code{\link{F.dfunc.estim}}), but both detection and
#' the additional site data are needed to estimate abundance (or to include
#' site-level covariates in the detection function; see
#' \code{\link{F.abund.estim}}).
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
#' @name thrasherSiteData
#' 
#' @docType data
#' 
#' @format A data.frame containing 120 rows and 6 columns.  Each row represents
#' a site (point) surveyed.  Column descriptions: \enumerate{ \item
#' \code{siteID}: Factor (120 levels), the site or point surveyed.  \item
#' \code{observer}: Factor (six levels), identity of the observer who surveyed
#' the point.  \item \code{bare}: Number, the mean bare ground cover (\%)
#' within 100 m of each point.  \item \code{herb}: Number, the mean herbaceous
#' cover (\%) within 100 m of each point.  \item \code{shrub}: Number, the mean
#' shrub cover (\%) within 100 m of each point.  \item \code{height}: Number,
#' the mean shrub height (cm) within 100 m of each point.  }
#' 
#' @seealso \code{\link{thrasherDetectionData}}
#' 
#' @source A subset of Jason Carlisle's dissertation data, University of Wyoming.
#' 
#' @references Carlisle, J.D. 2017. The effect of sage-grouse conservation on
#' wildlife species of concern: implications for the umbrella species concept.
#' Dissertation. University of Wyoming, Laramie, Wyoming, USA.
#' 
#' @keywords datasets
NULL