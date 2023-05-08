#' @title Effective Detection Radius (EDR) for estimated detection functions
#'   with point transects
#'   
#' @description Computes Effective Detection Radius (EDR) for estimated 
#'   detection functions with point transects.  The point-transect equivalent to
#'   Effective Strip Width (ESW).
#'   
#' @param obj An estimated detection function object.  An estimated detection 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} =
#'   1 is assumed.
#'   
#' @param newdata A data frame containing new values of the covariates at which
#'   EDR's are sought. If NULL or missing and 
#'   \code{obj} contains covariates, the covariates stored in \code{obj}
#'   are used.  See \bold{Value} section. 
#'   
#' @details The point-transect equivalent to Effective Strip Width (ESW).
#'   
#' @return If \code{newdata} is not missing and not NULL and 
#' covariates are present in \code{obj}, returned value is 
#' a vector with length equal to the number of rows in \code{newdata}. 
#' If \code{newdata} is missing or NULL and covariates are present
#' in \code{obj}, returned value is a vector with length equal to 
#' the number of detections in \code{obj$detections}. In either of the 
#' above cases, elements in the returned vector are 
#' the effective detection radii for the corresponding set of 
#' covariates.  
#' 
#' If \code{obj} does not contain covariates, \code{newdata} is ignored and 
#' a scalar equal to the (constant) effective detection radius for all 
#' detections is returned.  
#'   
#' @author Aidan McDonald, WEST Inc., \email{aidan@mcdcentral.org}\cr Trent
#'   McDonald, WEST Inc., \email{tmcdonald@west-inc.com}
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{ESW}}, 
#' \code{\link{effectiveDistance}}
#'   
#' @examples
#' # Load example thrasher data (point transect survey type)
#' data(thrasherDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1
#'                   , detectionData=thrasherDetectionData
#'                   , likelihood="halfnorm"
#'                   , w.hi=units::set_units(175, "m")
#'                   , pointSurvey=TRUE)
#' 
#' # Compute effective detection radius (EDR)
#' EDR(dfunc)
#' 
#' # EDR only applies to point transect surveys
#' # ESW is the line transect equivalent
#' # The effectiveDistance function tests whether the dfunc was
#' # fit to line or point data, and returns either ESW or EDR accordingly
#' effectiveDistance(dfunc)
#'   
#' @keywords modeling
#'   
#' @export

EDR <- function(obj, newdata){
  
  # Issue error if the input detection function was fit to line-transect data
  if(!(obj$pointSurvey)) stop("EDR is for point transects only.  See ESW for the line-transect equivalent.")
  
  like <- match.fun(paste( obj$like.form, ".like", sep=""))
  
  seq.length = 200  
  
  # Can't evaluate hazrate at 0
  if( (obj$like.form == "hazrate") & (obj$x.scl == obj$w.lo) ){
    x <- seq( obj$w.lo + 1e-6*(obj$w.hi - obj$w.lo), obj$w.hi, length=seq.length)
  } else {
    x <- seq( obj$w.lo, obj$w.hi, length=seq.length)
  }  

  
  
  if( !is.null(obj$covars) ){
    if(missing(newdata)){
      newdata <- NULL  # in this case, predict.dfunc will use obj$covars, but gotta have something to pass
    }
    
    params <- predict.dfunc(obj, newdata, type="parameters")
    
    # Use covars= NULL here because we evaluated covariates to get params above
    # after this, y is n X length(x).  each row is an unscaled distance 
    # function (f(x) = x*g(x)) 
    #
    # Note: pointSurvey parameter does not matter here because 
    # integration.constant is not called in likelihood (because unscaled)

    # seqy <- seqx * density( dist = seqx, scale = FALSE, w.lo = w.lo, w.hi = w.hi, a = a, expansions = expansions, ...)
    
    zero <- units::set_units(x = 0
                             , value = obj$outputUnits
                             , mode = "standard")

    x <- x - obj$w.lo
    y <- x * apply(X = params
                 , MARGIN = 1
                 , FUN = like
                 , dist = x
                 , series = obj$series
                 , covars = NULL
                 , expansions = obj$expansions
                 , w.lo = zero
                 , w.hi = obj$w.hi - obj$w.lo
                 , pointSurvey = obj$pointSurvey
                 , scale=FALSE)    
    y <- t(y)

    # Trapazoid rule.  
    dx <- units::drop_units(x[3]-x[2])
    y1 <- y[,-1,drop=FALSE]
    y  <- y[,-ncol(y),drop=FALSE]
    rho <- dx*rowSums(y + y1)/2
    rho <- sqrt(2*rho)
    rho <- units::set_units(rho, obj$outputUnits, mode = "standard")
    
  } else {
    # this returns (Integral xg(x)dx)/dist
    # integral is nrow(obj$detections) long vector
    integral <- integration.constant(dist = obj$detections$dist
                                   , density = like
                                   , a = obj$parameters
                                   , covars = obj$covars
                                   , w.lo = obj$w.lo
                                   , w.hi = obj$w.hi
                                   , expansions = obj$expansions
                                   , pointSurvey = obj$pointSurvey
                                   , series = obj$series
                                   )
    # obj$detections$dist is in denominator of integration.constant for point surveys. 
    # multiply here to remove it. vector inside root should be constant.
    rho <- sqrt(2 * integral * units::drop_units(obj$detections$dist))[1]
    
    # multiplying by obj$detections$dist in above line was a trick because we called 
    # integration.constant (because computations are complicated). But what about units? 
    # rho should have same units as obj$detections$dist, but one cannot take root of vectors with units. 
    # So, add back the units. 
    rho <- units::set_units(rho, obj$outputUnits, mode = "standard")
  
  }
  
  rho
}