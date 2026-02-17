#' @title Integrate Line-transect Triangle function 
#' 
#' @description
#' Compute exact integral of the triangle distance function for line 
#' transects. 
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' Returned integrals are
#' \deqn{\int_0^{w} (\frac{p}{\theta_i}I(0\leq x \leq \theta_i) + 
#'    \frac{1-p}{w - \theta_i}I(\theta_i < x \leq w)) dx = \frac{\theta_i}{p},}{
#' Integral((p/Theta)I(0<=x<=Theta) + ((1-p)/(w-Theta))I(Theta<x<=w)) = Theta / p,} 
#' where \eqn{w = w.hi - w.lo}, \eqn{\theta_i}{Theta} is the estimated one-step
#' distance function
#' threshold for the i-th observed distance, and \eqn{p}{p} is the estimated 
#' one-step proportion. 
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateHalfnormLines}} 
#' 
#' @examples
#' 
#' w <- 250
#' T <- 160
#' p <- 0.4
#' obj <- matrix(c(T,p), 1, 2)
#' 
#' integrateTriangleLines(obj
#'   , w.lo = units::set_units(0,"m")
#'   , w.hi = units::set_units(w,"m")
#'   , Units = "m")
#'   
#' # same
#' (1 + p) * T / 2 + p * (w - T)
#' 
#' # check by numeric integration
#' triLike <- function(d, T, p, wl, wh) { 
#'   y <- triangle.like(a = c(log(T), p)
#'            , dist = d - wl
#'            , covars = matrix(1, length(d))
#'            , w.hi = wh - wl)$L.unscaled
#'   y
#' }
#' integrate(triLike, lower = 0, upper = w, T = T, p = p, wl = 0, wh = w)
#'   
#' @export
#' 
integrateTriangleLines <- function(object
                                  , newdata = NULL
                                  , w.lo = NULL
                                  , w.hi = NULL
                                  , Units = NULL
                              ){

  if( inherits(object, "dfunc") ){
    Units <- object$outputUnits
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
  } 
  
  Theta <- setUnits(object[,1], Units)
  p <- object[,2]

  outArea <- (Theta - w.lo) * (1 + p) / 2 + # this is trapazoid part
    p * (w.hi - Theta) # horizontal part
  
  outArea 
  
}
