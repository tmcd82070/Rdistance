#' @name perpDists
#' @aliases perpDists
#' @title Compute off-transect ditances from sighting distances and angles
#' @description Computes off-transect (also called 'perpendicular') distances from measures of sighting distance and sighting angle.
#' @usage perpDists(sightDist, sightAngle, data)
#' @param sightDist Character, name of column in \code{data} that contains the observed or sighting distances from the observer to the detected objects.
#' @param sightAngle Character, name of column in \code{data} that contains the observed or sighting angles from the line transect to the detected 
#'   objects.  Angles must be measured in degrees.
#' @param data data.frame object containing sighting distance and sighting angle.
#' @details If observers recorded sighting distance and sighting angle (as is often common in line transect surveys), use this function to convert 
#'   to off-transect distances, the required input data for \code{F.dfunc.estim}.
#' @return A vector of off-transect (or perpendicular) distances.  Units are the same as \code{sightDist}.
#' @references Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L. 1993. 
#'   \emph{Distance Sampling: Estimating Abundance of Biological Populations}. Chapman and Hall, London.
#' @author Jason Carlisle, University of Wyoming and WEST Inc., \email{jcarlisle@west-inc.com}
#' @seealso \code{\link{dfuncEstim}}
#' @examples
#' # Load the example dataset of sparrow detections from package
#' data(sparrowDetectionData)
#' # Compute perpendicular, off-transect distances from the observer's sight distance and angle
#' sparrowDetectionData$perpDist <- perpDists(sightDist="sightdist", sightAngle="sightangle",
#'                                            data=sparrowDetectionData)
#' @export

perpDists <- function(sightDist, sightAngle, data){
  
  # Stop and print error if dist or angle is NA
  if(any(is.na(data[, sightDist])))
    stop("Please remove detections for which sightDist and/or sightAngle is NA.")
  if(any(is.na(data[, sightAngle])))
    stop("Please remove detections for which sightDist and/or sightAngle is NA.")
  
  # Convert degrees to radians, then apply sine function and round off decimal places
  data[, sightDist] * sin(data[, sightAngle]*(pi/180))
    
}