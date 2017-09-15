#' @name EDR
#' @aliases EDR
#' @title Effective Detection Radius (EDR) for estimated detection functions with point transects
#' @description Computes Effective Detection Radius (EDR) for estimated
#' detection functions with point transects.  The point-transect equivalent to
#' Effective Strip Width (ESW).
#' @usage EDR(obj)
#' @param obj An estimated detection function object.  An estimated detection
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} = 1
#'   is assumed.
#' @details The point-transect equivalent to
#' Effective Strip Width (ESW).
#' @return A scalar equal to the area under the detection function from \code{obj$w.lo} to \code{obj$w.hi}.
#' @author Aidan McDonald, WEST Inc., \email{aidan@mcdcentral.org}
#' @seealso \code{\link{dfuncEstim}} \code{\link{ESW}}
#' @keywords modeling
#' @export

EDR <- function(obj){
  
  # Issue error if the input detection function was fit to line-transect data
  if(!(obj$pointSurvey)) stop("EDR is for point transects only.  See ESW for the line-transect equivalent.")
  
  integral <- integration.constant(obj$dist,
                                   match.fun(paste( obj$like.form, ".like", sep="")),
                                   obj$w.lo, obj$w.hi, covars = obj$covars,
                                   obj$parameters, obj$expansions,
                                   pointSurvey = obj$pointSurvey,
                                   series = obj$series)
  
  rho <- sqrt(2*integral*obj$dist)[1]
  
  rho
}