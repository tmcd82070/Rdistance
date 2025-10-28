#' @title Integrate Half-normal Point transects
#' 
#' @description
#' Compute integral of the half-normal distance function for 
#' point surveys.
#' 
#' @inheritParams effectiveDistance
#' 
#' @details 
#' Returned integral is 
#' \deqn{\int_0^{w.hi} xe^{-x^2/2\sigma^2} dx}{Integral(xe^{-x^2/2\sigma^2})}, 
#' which equals,
#' \deqn{0.5\sigma^2(1 - e^{-w^2/2\sigma^2})}{0.5*s^2*(1 - exp(-w^2/2*s^2))}.
#' 
#' @inherit integrateHalfnormLines return
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexp}}; 
#' \code{\link{integrateOneStepPoints}} 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(3.5, -0.5)
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , par = beta 
#'   , likelihood = "halfnorm"
#'   , w.lo = units::set_units(w.lo, "m")
#'   , w.hi = units::set_units(w.hi, "m")
#' )
#' class(ml) <- "dfunc"
#' integrateHalfnormPoints(ml)
#' 
#' # Check: Integral of x exp(-x^2/(2*s^2)) from 0 to w = w.hi-w.lo
#' sigma <- exp(c(beta[1], beta[1] + beta[2]))
#' w <- w.hi - w.lo
#' intgral <- sigma^2 * (1 - exp(-w^2 / (2*sigma^2)))
#' intgral
#' 
#' # Effective detection radius
#' sqrt(2 * intgral)
#' 
#' @export
#' 
integrateHalfnormPoints <- function(object
                            , newdata = NULL
                            , w.lo = NULL
                            , w.hi = NULL
                            , Units = NULL
                              ){

  if( inherits(object, "dfunc") ){
    Units <- object$outputUnits
    w.lo <- object$w.lo
    w.hi <- object$w.hi
    object <- stats::predict(object = object
                             , newdata = newdata
                             , type = "parameters"
    )
  } 
  
  # Drop units b/c pnorm hickups when y is vector 
  # (but not matrix, interesting...?)
  # It is safe to drop units b/c we converted everything 
  # to same units in parseModel.
  
  w <- units::set_units(w.hi - w.lo, NULL)
  s.squared <- object^2

  outArea <- s.squared * (1 - exp(-w^2 / (2*s.squared)))
  
  outArea <- units::set_units(outArea
                              , Units
                              , mode = "standard")
  
  outArea 
  
}