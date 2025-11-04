#' @title Integrate Hazard-rate line survey distance functions
#' 
#' @description
#' Compute integral of the hazard-rate distance function for 
#' line-transect surveys.
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' 
#' Returned integrals are
#' \deqn{\int_0^{w} (1 - e^{-(x/\sigma_i)^{-k}}) dx = w - \frac{\sigma_i}{k} \Gamma(-\frac{1}{k}, {\frac{\sigma_i}{w}}^{k}),}{
#' Integral( 1 - e^{-(x/\sigma_i)^(-k)} ) = w - (s/k)(Gamma(-1/k, (s/w)^k),}
#' where \eqn{w = w.hi - w.lo}, \eqn{\sigma_i}{s} and \eqn{k}{k} are estimated 
#' hazard-rate distance 
#' function parameters for the 
#' i-th observed distance, and \eqn{\Gamma()}{Gamma()} is the incomplete gamma 
#' function. 
#' Rdistance uses the 
#' incomplete gamma function implemented in 
#' \code{\link[expint]{gammainc}}, which for 
#' all intents and purposes is exact.
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateOneStepLines}} 
#' 
#' @examples
#' 
#' # A pre-estimated hazard rate distance function: sparrowDfuncObserver
#' fit <- sparrowDfuncObserver
#' table(ESW(fit))
#' table(integrateHazrateLines(fit))
#' 
#' # Check: Integral of 1 - exp(-(x/s)^(-k)) from 0 to w.hi-w.lo
#' w <- units::set_units(fit$w.hi - fit$w.lo, NULL)
#' params <- predict(fit)
#' sigma <- params[,1]
#' minusk <- -params[,2]
#' 
#' outArea <- w + sigma * 
#'            expint::gammainc(1/minusk, (w/sigma)^(minusk)) / minusk
#' table(outArea)
#' 
#' @export
#' 
integrateHazrateLines <- function(object
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

  outArea <- w + sigma * expint::gammainc(1/minusk, (w/sigma)^(minusk)) / minusk
    
  outArea <- units::set_units(outArea
                              , Units
                              , mode = "standard")
  
  outArea 
  
}