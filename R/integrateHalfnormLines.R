#' @title Integrate Half-normal line surveys
#' 
#' @description
#' Compute integral of the half-normal distance function for 
#' line-transect surveys.
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' Returned integrals are
#' \deqn{\int_0^{w} e^{-x^2/2\sigma_i^2} dx = \sqrt{2\pi}\sigma_i (\Phi(w) - 0.5),}{
#' Integral(e^{-x^2/(2s^2)}) = sqrt(2*pi)*s*(Pnorm(w) - 0.5),}
#' where \eqn{w = w.hi - w.lo}, \eqn{\sigma_i}{s} is the estimated half-normal distance 
#' function parameter for the 
#' i-th observed distance, and \eqn{\Phi}{Pnorm} is the standard normal 
#' cumulative probability function. 
#' Rdistance uses R's base 
#' function \code{pnorm()}, which for all intents and purposes is exact.
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateOneStepLines}} 
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
#' integrateHalfnormLines(ml)
#' 
#' # Check: Integral of exp(-x^2/(2*s^2)) from 0 to w.hi-w.lo
#' b <- exp(c(beta[1], beta[1] + beta[2]))
#' intgral <- (pnorm(w.hi, mean=w.lo, sd = b) - 0.5) * sqrt(2*pi)*b
#' intgral
#' 
#' 
#' @export
#' 
integrateHalfnormLines <- function(object
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
  
  w.lo <- dropUnits(w.lo)
  w.hi <- dropUnits(w.hi)
  
  outArea <- (stats::pnorm(q = w.hi
                  , mean = w.lo
                  , sd = object) - 0.5) * sqrt(2*pi)*object
  
  outArea <- setUnits(outArea, Units)
  
  outArea 
  
}