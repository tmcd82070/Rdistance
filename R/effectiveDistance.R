#' @name effectiveDistance
#' @aliases effectiveDistance
#' @title Calculates the ESW or EDR for estimated detection functions
#' @description Computes Effective Strip Width (ESW) for line-transect detection
#' functions, or the analagous Effective Detection Radius (EDR) point-transect
#' detection functions.
#' @usage effectiveDistance(obj, covars = NULL)
#' @param obj An estimated detection function object.  An estimated detection
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} = 1
#'   is assumed.
#' @param covars Covariate values for which to calculate ESW.
#' @details Serves as a wrapper for \code{\link{ESW}} and \code{\link{EDR}}.
#' @return A scalar equal to the area under the detection function from \code{obj$w.lo} to \code{obj$w.hi}.
#' @seealso \code{\link{dfuncEstim}} \code{\link{ESW}} \code{\link{EDR}}
#' @keywords modeling
#' @export

effectiveDistance <- function(obj, covars = NULL){
  
  # call ESW for line transects and EDR for point transects

  if (obj$pointSurvey) {
    EDR(obj)
  } else {
    ESW(obj, covars)
  }

}