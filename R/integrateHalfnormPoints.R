#' @title Integrate Half-normal Point transects
#' 
#' @description
#' Compute integral of the half-normal distance function for 
#' point surveys.
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' Returned integrals are 
#' \deqn{\int_0^{w} xe^{-x^2/2\sigma_i^2} dx = 0.5\sigma_i^2(1 - e^{-w^2/2\sigma_i^2}),}{
#' Integral(xe^{-x^2/(2s^2)}) = 0.5*s^2*(1 - exp(-w^2/(2*s^2))),} 
#' where \eqn{w = w.hi - w.lo} and \eqn{\sigma_i}{s} is the estimated half-normal 
#' distance function parameter for the i-th observed distance.  
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpPoints}}; 
#' \code{\link{integrateOneStepPoints}} 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- rep(1,4) %m%. # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(3.5, -0.5)
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , par = beta 
#'   , likelihood = "halfnorm"
#'   , w.lo = w.lo %#% "m"
#'   , w.hi = w.hi %#% "m"
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
  
  w <- w.hi - w.lo
  object <- setUnits(object, Units)
  
  # exp() can't handle units, even if they are [1], gotta remove em
  s.squared <- object^2 # want units on this one
  wsRatio <- dropUnits(-w^2 / (2*s.squared)) # remove [1] units
  
  outArea <- s.squared * (1 - exp( wsRatio ))
  
  outArea 
  
}