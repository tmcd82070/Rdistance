#' @name perp.dists
#' @alias perp.dists
#' @title Compute off-transect ditances from sighting distances and angles
#' @description Computes off-transect (also called 'perpendicular') distances from measures of sighting distance and sighting angle.
#' @usage perp.dists(s.dist, s.angle, data)
#' @param s.dist Character, name of column in \code{data} that contains the observed or sighting distances from the observer to the detected objects.
#' @param s.angle Character, name of column in \code{data} that contains the observed or sighting angles from the line transect to the detected 
#'   objects.  Angles must be measured in degrees.
#' @param data data.frame object containing sighting distance and sighting angle.
#' @details If observers recorded sighting distance and sighting angle (as is often common in line transect surveys), use this function to convert 
#'   to off-transect distances, the required input data for \code{F.dfunc.estim}.
#' @value A vector of off-transect (or perpendicular) distances.  Units are the same as \code{obs.dist}.
#' @references Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L. 1993. 
#'   \emph{Distance Sampling: Estimating Abundance of Biological Populations}. Chapman and Hall, London.
#' @author Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}
#' @seealso \code{\link{F.dfunc.estim}}
#' @examples # Load the example dataset of sparrow detections from package
#' data(sparrow.detections)
#' # Compute perpendicular, off-transect distances from the observer's sight distance and angle
#' sparrow.detections$dist <- perp.dists(s.dist="sightdist", s.angle="sightangle",
#'                                       data=sparrow.detections)
#' # Proceed to fitting the detection function with F.dfunc.estim

perp.dists <- function(s.dist, s.angle, data){
  
  # Stop and print error if dist or angle is NA
  if(any(is.na(data[, s.dist])))
    stop("Please remove detections for which s.dist and/or s.angle is NA.")
  if(any(is.na(data[, s.angle])))
    stop("Please remove detections for which s.dist and/or s.angle is NA.")
  
  # Convert degrees to radians, then apply sine function and round off decimal places
  data[, s.dist] * sin(data[, s.angle]*(pi/180))
    
}