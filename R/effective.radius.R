#' @export

effective.radius <- function(x){
  
  integral <- integration.constant(x$dist, match.fun(paste( x$like.form, ".like", sep="")), x$w.lo, x$w.hi, covars = x$covars, x$parameters, x$expansions, point.transects = x$point.transects, series = x$series)
  
  rho <- sqrt(2*integral*x$dist)[1]
  
  rho
}