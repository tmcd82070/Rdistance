#' @name sparrowDf 
#' 
#' @title Brewer's Sparrow detection data frame
#' in Rdistance >4.0.0 format.
#' 
#' @description 
#' Detection data from line transect surveys for Brewer's sparrow on 72 transects 
#' located on a 4105 km^2 study area in central Wyoming collected by 
#' Dr. Jason Carlisle as part of his graduate work in the Wyoming Cooperative Fish & Wildlife
#' Research Unit in 2012. Each transect was 500 meters long.
#' 
#' @docType data
#' 
#' @format A rowwise tibble containing 72 rows and 9 columns, one of which 
#' is nested data frame of detections.  Each row represents
#' one transect.  The embedded data frame in column \code{detections} 
#' contains the detections made on the transect represented on that row. 
#' 
#' Column descriptions: 
#' \enumerate{ 
#'   \item \code{siteID}: Factor (72 levels), the transect identifier 
#'   for that row of the data frame. 
#'   \item \code{length}: The length, in meters [m], of each transect.  
#'   \item \code{observer}: Identity of the observer who surveyed the transect.
#'   \item \code{bare}: The mean bare ground cover (\%) within 100 [m] of the transect.
#'   \item \code{herb}: The mean herbaceous cover (\%) within 100 [m] of the transect.
#'   \item \code{shrub}: The mean shrub cover (\%) within 100 [m] of the transect.
#'   \item \code{height}: The mean shrub height [cm] within 100 [m] of the transect.
#'   \item \code{shrubclass}: Shrub class factor.  Either "Low"" when 
#'   shrub cover is < 10\%, or "High" if cover >= 10\%.  
#' }
#'  
#' The embedded data frame in column \code{detections} contains the following
#' variables:
#' \enumerate{
#'   \item \code{groupsize}: The number of individuals in the detected group.  
#'   \item \code{sightdist}: Distance [m] from observer to the detected group.  
#'   \item \code{sightangle}: Angle [degrees] from the transect 
#'   line to the detected group. Not bearing. Range 0 [degrees] to 90 [degrees].
#'   \item \code{dist}: Perpendicular, off-transect distance [m], from the
#'   transect to the detected group.  This is the distance used in analysis.
#'   Calculated using \code{\link{perpDists}}.  
#' }
#' 
#' @seealso \code{\link{sparrowSiteData}}, \code{\link{sparrowDetectionData}},
#' \code{\link{RdistDf}}
#' 
#' @source The Brewer's sparrow data are a subset of data collected 
#' by Jason Carlisle and various field technicians for his Ph.D. from the 
#' Department of Ecology, University of Wyoming, in 2017.  This portion of 
#' Jason's work was funded by the Wyoming Game and Fish Department through agreements 
#' with the University of Wyoming's  Cooperative Fish & Wildlife
#' Research Unit (2012). 
#' 
#' @examples
#' data(sparrowDf)
#' tidyr::unnest(sparrowDf, detections)  # only non-zero transects
#' Rdistance::unnest(sparrowDf) # with zero transects at the bottom
#' summary(sparrowDf,
#'   formula = dist ~ groupsize(groupsize)
#' )
#' 
#' @references Carlisle, J.D. 2017. The effect of sage-grouse conservation on
#' wildlife species of concern: implications for the umbrella species concept.
#' Dissertation. University of Wyoming, Laramie, Wyoming, USA.
#' 
#' Carlisle, J. D., and A. D. Chalfoun. 2020. The abundance of Greater 
#' Sage-Grouse as a proxy for the abundance of sagebrush-associated songbirds 
#' in Wyoming, USA. \emph{Avian Conservation and Ecology} 15(2):16. 
#' \doi{10.5751/ACE-01702-150216}
#' 
#' @keywords datasets
NULL