#' @title Calculates the effective sampling distance for 
#' estimated detection functions.
#' 
#' @description Computes Effective Strip Width (ESW) for line-transect detection
#'   functions, or the analagous Effective Detection Radius (EDR) for point-transect
#'   detection functions.
#'   
#' @param obj An estimated detection function object.  An estimated detection 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} =
#'   1 is assumed.
#'   
#' @param newdata A data frame containing new values of the covariates at which
#'   ESW's or EDR's are sought. If NULL or missing, the covariates stored in
#'   \code{obj} are used. If covariates are present, one effective distance is
#'   returned for each detection. If covariates are not present, one effective
#'   distance is produced.
#'  
#' @details Serves as a wrapper for \code{\link{ESW}} and \code{\link{EDR}}.
#' 
#' @return If covariates are present in \code{obj}, a vector of 
#' length equal to the number of detections in \code{obj$dist} containing
#' separate effective distances for each detection. If 
#' \code{obj} does not contain covariates, a scalar 
#' equal to the (constant) effective sampling distance for all detections.  
#'      
#' @seealso \code{\link{dfuncEstim}} \code{\link{ESW}} \code{\link{EDR}}
#' @keywords modeling
#' @export

effectiveDistance <- function(obj, newdata = NULL){
  
  # call ESW for line transects and EDR for point transects

  if (obj$pointSurvey) {
    EDR(obj)  # (jdc)  Trent, I removed the newdata argument here to get some other debugging done.
  } else {
    ESW(obj,newdata)
  }

}