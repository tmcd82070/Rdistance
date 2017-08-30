#' @name effective.radius
#' @aliases effective.radius
#' @title Effective Transect Radius for estimated detection functions with point transects
#' @description Computes effective transect radius for estimated detection functions with point transects.
#' @usage effective.radius(x)
#' @param obj An estimated detection function object.  An estimated detection
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{F.dfunc.estim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} = 1
#'   is assumed.
#' @details The equivalent of Effective Strip Width for point transects.
#' @return A scalar equal to the area under the detection function from \code{obj$w.lo} to \code{obj$w.hi}.
#' @author Aidan McDonald, WEST, Inc., \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}} \code{\link{ESW}}
#' @keywords modeling
#' @export

effective.radius <- function(x){
  
  integral <- integration.constant(x$dist, match.fun(paste( x$like.form, ".like", sep="")), x$w.lo, x$w.hi, covars = x$covars, x$parameters, x$expansions, point.transects = x$point.transects, series = x$series)
  
  rho <- sqrt(2*integral*x$dist)[1]
  
  rho
}