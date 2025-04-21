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
#' @seealso [integrateNumeric()]; [integrateNegexp()]; 
#' [integrateOneStep()] 
#' 
#' @examples
#' 
#' # Fake a distance function object
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' ml <- list(
#'     mf = model.frame(d ~ obs) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#'   , outputUnits = units(units::set_units(1,"m"))
#'   , transType = "line"
#' )
#' class(ml) <- "dfunc"
#' integrateHalfnorm(ml)
#' 
#' # Check:
#' w.hi <- 125
#' w.lo <- 0
#' s1 <- 40
#' s2 <- exp(log(s1) + log(0.5))
#' obs1Scaler <- (pnorm(w.hi, mean=w.lo, sd = s1) - 0.5) * sqrt(2*pi)*s1
#' obs2Scaler <- (pnorm(w.hi, mean=w.lo, sd = s2) - 0.5) * sqrt(2*pi)*s2
#' c(obs1Scaler, obs2Scaler)
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
  
  outArea <- (pnorm(q = w.hi
                  , mean = w.lo
                  , sd = y) - 0.5) * sqrt(2*pi)*y
  
  outArea <- units::set_units(outArea
                              , object$outputUnits
                              , mode = "standard")
  
  outArea 
  
}