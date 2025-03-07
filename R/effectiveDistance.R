#' @title effectiveDistance - Effective sampling distances
#' 
#' @description Computes Effective Strip Width (ESW) for line-transect detection
#'   functions, or the analogous Effective Detection Radius (EDR) for point-transect
#'   detection functions.
#'
#' @param newdata A data frame containing new values for 
#' covariates at which either
#' ESW's or EDR's will be computed. If NULL and 
#'   \code{object} contains covariates, the  
#'   covariates stored in
#'   \code{object} are used (like \code{\link{predict.lm}}).
#'   If not NULL, covariate values in \code{newdata}
#'   are used. 
#'   See \bold{Value} section for more information. 
#'  
#' @inheritParams predict.dfunc 
#'   
#' @details Serves as a wrapper for 
#' \code{\link{ESW}} and \code{\link{EDR}}.
#' 
#' @return If \code{newdata} is present, the returned value is 
#' a vector of effective sampling distances for values of the 
#' covariates in \code{newdata} with length equal to 
#' the number of rows in \code{newdata}. 
#' If \code{newdata} is NULL, the returned value is a vector of effective
#' sampling distances associated with covariate values in \code{object} and has 
#' the same number of detected groups.  The returned vector 
#' has measurement units, i.e., \code{object$outputUnits}.
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