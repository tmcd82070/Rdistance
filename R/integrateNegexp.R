#' @title Integrate Negative exponential 
#' 
#' @description
#' Compute integral of the negative exponential distance function. 
#' 
#' @inheritParams effectiveDistance
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
integrateNegexp <- function(object
                            , newdata = NULL
                              ){

  y <- stats::predict(object = object
                      , newdata = newdata
                      , type = "parameters"
  )
  
  # Remove units b/c cannot exp units object.
  
  w.lo <- units::set_units(object$w.lo, NULL)
  w.hi <- units::set_units(object$w.hi, NULL)
  
  outArea <- (1 - exp(-y*(w.hi - w.lo))) / y
  
  outArea <- units::set_units(outArea
                              , object$outputUnits
                              , mode = "standard")
  
  outArea 
  
}