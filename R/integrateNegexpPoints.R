#' @title Integrate Negative exponential point surveys
#' 
#' @description
#' Compute integral of the negative exponential distance function
#' for point surveys
#' 
#' @inheritParams effectiveDistance
#' 
#' @inherit integrateHalfnormLines return
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateHazratePoints}} 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(-5, -0.5)
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , par = beta 
#'   , likelihood = "negexp"
#'   , w.lo = units::set_units(w.lo, "m")
#'   , w.hi = units::set_units(w.hi, "m")
#' )
#' class(ml) <- "dfunc"
#' integrateNegexp(ml)
#' 
#' # Check: Integral of exp(-bx) from 0 to w.hi-w.lo
#' b <- c(exp(beta[1]), exp(beta[1] + beta[2]))
#' intgral <- (1 - exp(-b*(w.hi - w.lo))) / b
#' intgral
#' 
#' 
#' @export
#' 
integrateNegexpPoints <- function(object
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
  
  # Remove units b/c cannot exp units object.
  w <- units::set_units(w.hi - w.lo, NULL)
  aw <- object * w

  outArea <- (1 - exp(-aw) * (aw + 1)) / (object * object)
  
  outArea <- units::set_units(outArea
                              , Units
                              , mode = "standard")
  
  outArea 
  
}