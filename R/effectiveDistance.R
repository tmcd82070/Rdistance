#' @title Effective sampling distances
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
#' Effective distances are areas under scaled 
#' distance functions (i.e., area under g(x)). Areas are 
#' exact for functions whose integral is known (e.g., negexp). 
#' Numeric integration is used for all others. 
#' 
#' @return If \code{newdata} is present, the returned value is 
#' a vector of effective sampling distances associated with 
#' covariate values in \code{newdata}. Length of return 
#' in this case is the number of rows in \code{newdata}. 
#' If \code{newdata} is NULL, the returned value is a vector 
#' of effective sampling distances associated with covariate 
#' values in \code{object}. Length of return in this case 
#' is the number of detected groups.  The returned vector 
#' has measurement units, i.e., \code{object$outputUnits}.
#' 
#'      
#' @seealso \code{\link{dfuncEstim}},
#'   \code{\link{ESW}},
#'   \code{\link{EDR}}, 
#'   \code{\link{integrateNumeric}},
#'   \code{\link{integrateNegexp}}
#' 
#' @export

effectiveDistance <- function(object, newdata = NULL){
  
  # call ESW for line transects and EDR for point transects

  if (is.points(object)) {
    EDR(object, newdata)
  } else {
    ESW(object, newdata)
  }

}