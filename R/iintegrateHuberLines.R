#' @title Integrate Line-transect Huber function 
#' 
#' @description
#' Compute exact integral of the 'huber' distance function for line 
#' transects. 
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
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
integrateHuberLines <- function(object
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
  
  one    <- setUnits(1, "1")
  theta1 <- setUnits(object[,1], Units)
  theta2 <- setUnits(object[,2], Units)
  p      <- setUnits(object[,3], "1")
  Theta  <- theta1 + theta2
  m      <- theta1*(Theta - 0.5*theta1)

  part1 <- theta1 - (one - p)*theta1^3 / (6*m)   # 0 to theta1
  part2 <- theta2 * ((theta1^2 / (2*m)) + p) / 2 # theta1 to theta1 + theta2 (a trapazoid)
  part3 <- p * (w.hi - w.lo - Theta)             # theta1 + theta2 to w (a rectangle)

  outArea <- part1 + part2 + part3  # units should be Units (linear)
  
  outArea 
  
}
