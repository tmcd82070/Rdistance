#' @title Calculates the effective sampling distance for 
#' estimated detection functions
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
#'   ESW's or EDR's are sought. If NULL or missing and 
#'   \code{obj} contains covariates, the  
#'   covariates stored in
#'   \code{obj} are used. See \bold{Value} section. 
#'  
#' @details Serves as a wrapper for \code{\link{ESW}} and \code{\link{EDR}}.
#' 
#' @return If \code{newdata} is not missing or NULL and 
#' covariates are present in \code{obj}, returned value is 
#' a vector with length equal to the number of rows in \code{newdata}. 
#' If \code{newdata} is missing or NULL and covariates are present
#' in \code{obj}, returned value is a vector with length equal to 
#' the number of detections in \code{obj$dist}. In either of the 
#' above cases, elements in the returned vector are 
#' the effective sampling distances for the corresponding set of 
#' covariates.  
#' 
#' If \code{obj} does not contain covariates, \code{newdata} is ignored and 
#' a scalar equal to the (constant) effective sampling distance for all 
#' detections is returned.  
#'      
#' @seealso \code{\link{dfuncEstim}} \code{\link{ESW}} \code{\link{EDR}}
#' @keywords modeling
#' @export

effectiveDistance <- function(obj, newdata = NULL){
  
  # call ESW for line transects and EDR for point transects

  if (obj$pointSurvey) {
    EDR(obj, newdata)
  } else {
    ESW(obj, newdata)
  }

}