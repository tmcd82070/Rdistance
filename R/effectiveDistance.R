#' @title Effective sampling distances
#' 
#' @description Computes Effective Strip Width (ESW) for line-transect detection
#'   functions, or the analogous Effective Detection Radius (EDR) for point-transect
#'   detection functions.
#'
#' @param newdata A data frame containing new values for 
#' covariates at which either
#' ESW's or EDR's will be computed. If NULL and 
#'   `object` contains covariates, the  
#'   covariates stored in
#'   `object` are used (like [predict.lm()]).
#'   If not NULL, covariate values in `newdata`
#'   are used. 
#'   See **Value** section for more information. 
#'  
#' @inheritParams predict.dfunc 
#'   
#' @details Serves as a wrapper for 
#' [ESW()] and [EDR()].
#' 
#' Effective distances are areas under scaled 
#' distance functions (i.e., area under g(x)). Areas are 
#' exact for functions whose integral is known (e.g., negexp). 
#' Numeric integration is used for all others. 
#' 
#' @return If `newdata` is present, the returned value is 
#' a vector of effective sampling distances associated with 
#' covariate values in `newdata`. Length of return 
#' in this case is the number of rows in `newdata`. 
#' If `newdata` is NULL, the returned value is a vector 
#' of effective sampling distances associated with covariate 
#' values in `object`. Length of return in this case 
#' is the number of detected groups.  The returned vector 
#' has measurement units, i.e., `object$outputUnits`.
#' 
#'      
#' @seealso [dfuncEstim()],
#'   [ESW()],
#'   [EDR()], 
#'   [integrateNumeric()],
#'   [integrateNegexpLines()]
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
