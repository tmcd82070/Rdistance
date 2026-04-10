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
#' T1 <- 160
#' T2 <- 20
#' p <- 0.03
#' obj <- matrix(c(T1,T2,p), 1, 3)
#' 
#' integrateHuberLines(obj
#'   , w.lo = units::set_units(0,"m")
#'   , w.hi = units::set_units(w,"m")
#'   , Units = "m")
#'   
#' # same
#' huber.cumFunc(w,T1,T2,p,w)
#' 
#' # check by numeric integration
#' hubLike <- function(d, T1, T2, p, wl, wh) { 
#'   y <- huber.like(a = c(log(T1), T2, p)
#'            , dist = d - wl
#'            , covars = matrix(1, length(d))
#'            , w.hi = wh - wl)$L.unscaled
#'   y
#' }
#' integrate(hubLike, lower = 0, upper = w, T1 = T1
#'         , T2 = T2, p = p, wl = 0, wh = w)
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
    
    y <- stats::predict(object, newdata = newdata, type="dfunc")
    
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
  } 
  
  theta1 <- setUnits(object[,1], Units)
  theta2 <- setUnits(object[,2], Units)
  p      <- setUnits(object[,3], "1")
  w      <- rep(w.hi - w.lo, nrow(object))

  outArea <- huber.cumFunc(x = w
                         , t1 = theta1 
                         , t2 = theta2 
                         , p = p 
                         , w = w
  )

  outArea 
  
}
