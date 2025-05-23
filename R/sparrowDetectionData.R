#' @name sparrowDetectionData
#' 
#' @title Brewer's Sparrow detection data 
#' 
#' @description 
#' Detection data from line transect surveys for 
#' Brewer's sparrow on 72 transects 
#' located on a 4105 km^2 study 
#' area in central Wyoming. Data were collected by 
#' Dr. Jason Carlisle of the Wyoming Cooperative Fish & Wildlife
#' Research Unit in 2012. Each transect was 500 meters long.
#' 
#' @docType data
#' 
#' @format A data.frame containing 356 rows and 5 columns.  Each row represents
#' a detected group of sparrows.  Column descriptions: 
#' \enumerate{ 
#'   \item \code{siteID}: Factor (72 levels), the site or transect where the detection
#'   was made.  
#'   \item \code{groupsize}: Number, the number of individuals within
#'   the detected group.  
#'   \item \code{sightdist}: Number, distance (m) from the observer to the detected group.  
#'   \item \code{sightangle}: Number, the angle (degrees) from the transect 
#'   line to the detected group.  
#'   \item \code{dist}: Number, the perpendicular, off-transect distance (m) from the
#'   transect to the detected group.  This is the distance used in analysis.
#'   Calculated using \code{\link{perpDists}}.  }
#' 
#' @seealso \code{\link{sparrowSiteData}}
#' 
#' @source The Brewer's sparrow data are a subset of the data collected 
#' by Jason Carlisle and various field technicians for his Ph.D. from the 
#' Department of Ecology, University of Wyoming, in 2017.  This portion of 
#' Jason's work was funded by the Wyoming Game and Fish Department through agreements 
#' with the University of Wyoming's  Cooperative Fish & Wildlife
#' Research Unit (2012). 
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