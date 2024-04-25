#' @title effectiveDistance - Effective sampling distances
#' 
#' @description Computes Effective Strip Width (ESW) for line-transect detection
#'   functions, or the analogous Effective Detection Radius (EDR) for point-transect
#'   detection functions.
#'
#' @param newdata A data frame containing new values for 
#' covariates at which either
#' ESW's or EDR's will be computed. If NULL and 
#'   \code{x} contains covariates, the  
#'   covariates stored in
#'   \code{x} are used (like \code{\link{predict.lm}}).
#'   If not NULL, covariate values in \code{newdata}
#'   are used. 
#'   See \bold{Value} section for more information. 
#'  
#' @inheritParams predict.dfunc 
#'   
#' @details Serves as a wrapper for 
#' \code{\link{ESW}} and \code{\link{EDR}}.
#' 
#' @return If \code{newdata} is not missing or NULL and 
#' covariates are present in \code{x}, returned value is 
#' a vector with length equal to the number of rows in \code{newdata}. 
#' If \code{newdata} is missing or NULL and covariates are present
#' in \code{x}, returned value is a vector with length equal to 
#' the number of detections in \code{x}. In both cases, 
#' elements in the returned vector are 
#' the effective sampling distances for the corresponding set of 
#' covariates.  
#' 
#'      
#' @seealso \code{\link{dfuncEstim}} \code{\link{ESW}} \code{\link{EDR}}
#' 
#' @keywords modeling
#' @export

effectiveDistance <- function(object, newdata = NULL){
  
  # call ESW for line transects and EDR for point transects

  if (is.points(object)) {
    EDR(object, newdata)
  } else {
    ESW(object, newdata)
  }

}