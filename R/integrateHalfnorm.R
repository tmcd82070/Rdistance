#' @title Integrate Half-normal 
#' 
#' @description
#' Compute integral of the half-normal distance function. 
#' 
#' @inheritParams effectiveDistance
#' 
#' @details 
#' Returned integral is computed using R's base 
#' function \code{pnorm()}, which is very accurate.
#' 
#' @return A vector of areas under distance functions. 
#' If \code{newdata} is specified, return length is 
#' \code{nrow(newdata)}.  If \code{newdata} is NULL, 
#' return length is \code{length(distances(object))}. 
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexp}}; 
#' \code{\link{integrateOneStep}} 
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
#' integrateHalfnorm(ml)
#' 
#' # Check: Integral of exp(-x^2/(2*s^2)) from 0 to w.hi-w.lo
#' b <- exp(c(beta[1], beta[1] + beta[2]))
#' intgral <- (pnorm(w.hi, mean=w.lo, sd = b) - 0.5) * sqrt(2*pi)*b
#' intgral
#' 
#' 
#' @export
#' 
integrateHalfnorm <- function(object
                            , newdata = NULL
                              ){

  y <- stats::predict(object = object
                      , newdata = newdata
                      , type = "parameters"
  )
  
  # Drop units b/c pnorm hickups when y is vector 
  # (but not matrix, interesting...?)
  # It is safe to drop units b/c we converted everything 
  # to same units in parseModel.
  
  w.lo <- units::set_units(object$w.lo, NULL)
  w.hi <- units::set_units(object$w.hi, NULL)
  
  outArea <- (stats::pnorm(q = w.hi
                  , mean = w.lo
                  , sd = y) - 0.5) * sqrt(2*pi)*y
  
  outArea <- units::set_units(outArea
                              , object$outputUnits
                              , mode = "standard")
  
  outArea 
  
}