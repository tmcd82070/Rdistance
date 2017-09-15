#' @name effective.radius
#' @aliases effective.radius
#' @title Effective Transect Radius for estimated detection functions with point transects
#' @description Computes effective transect radius for estimated detection functions with point transects.
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

effective.radius <- function(obj){
  
  # Issue error if the input detection function was fit to line-transect data
  # Eventually, it would be nice to build the effective.radius code into ESW, so ESW could handle either.
  if(!(obj$point.transects)) stop("effective.radius is for point transects only.  See ESW for the line-transect equivalent.")
  
  integral <- integration.constant(obj$dist, match.fun(paste( obj$like.form, ".like", sep="")), obj$w.lo, obj$w.hi, covars = obj$covars, obj$parameters, obj$expansions, point.transects = obj$point.transects, series = obj$series)
  
  rho <- sqrt(2*integral*obj$dist)[1]
  
  rho
}