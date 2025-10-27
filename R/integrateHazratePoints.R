#' @title Integrate Hazard-rate line survey distance functions
#' 
#' @description
#' Compute integral of the hazard-rate distance function for 
#' line-transect surveys.
#' 
#' @inheritParams effectiveDistance
#' 
#' @details 
#' Returned integral is computed using the 
#' incomplete gamma function implementation in 
#' \code{\link{expint::gammainc()}}, which for all intents and purposes 
#' is exact.
#' 
#' @inherit integrateHalfnormLines return
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexp}}; 
#' \code{\link{integrateOneStepLines}} 
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
integrateHazratePoints <- function(object
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
  
  w <- units::set_units(w.hi - w.lo, NULL)
  sigma <- object[,1]
  minusk <- -object[,2]

  part1 <- w * w / 2
  part2 <- sigma * sigma * expint::gammainc(2/minusk, (w/sigma)^(minusk)) / minusk
  outArea <- part1 + part2
    
  outArea <- units::set_units(outArea
                              , Units
                              , mode = "standard")
  
  outArea 
  
}