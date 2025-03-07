#' @title EDR - Effective Detection Radius (EDR) for point transects
#'   
#' @description Computes Effective Detection Radius (EDR) for estimated 
#'   detection functions on point transects.  
#'   See \code{\link{ESW}} is for line transects. 
#'   
#' @inheritParams predict.dfunc
#'   
#' @details Effective Detection Radius is the integral under the 
#' detection function times distance. \if{latex}{I.e., 
#'     \deqn{EDR = \int_{w.lo}^{w.hi} xg(x)dx,} 
#'   where \eqn{g(x)} is the distance
#'   function scaled so that \eqn{g(x.scl) = g.x.scl}
#'   and \eqn{w.lo} and \eqn{w.hi} are the lower
#'   and upper truncation limits.  }
#'   
#' @inherit effectiveDistance return  
#' 
#' @inheritSection ESW Numeric Integration
#'    
#' @seealso \code{\link{dfuncEstim}}, \code{\link{ESW}}, 
#' \code{\link{effectiveDistance}}
#'   
#' @examples
#' # Load example thrasher data (point transect survey type)
#' data(thrasherDf)
#' 
#' # Fit half-normal detection function
#' dfunc <- thrasherDf |> dfuncEstim(formula=dist~bare)
#' 
#' # Compute effective detection radius (EDR)
#' EDR(dfunc) # vector length 192
#' effectiveDistance(dfunc) # same
#' EDR(dfunc, newdata = data.frame(bare=30)) # vector length 1
#'   
#' @keywords modeling
#'   
#' @importFrom stats predict
#' @export

EDR <- function(object, newdata = NULL){
  
  # Issue error if the input detection function was fit to line-transect data
  if( !Rdistance::is.points(object) ){
    stop("EDR is for point transects only.  See ESW for the line-transect equivalent.")
  } 
  
  nEvalPts <- checkNEvalPts(getOption("Rdistance_intEvalPts")) # MUST BE ODD!!!
  nInts <- nEvalPts - 1 # this will be even profided nEvalPts is odd
  seqx = seq(object$w.lo, object$w.hi, length=nEvalPts) 
  dx <- seqx[2] - seqx[1]  # or (w.hi - w.lo) / (nInts)
  
  y <- units::set_units(seqx, NULL) * 
       stats::predict(object = object
                    , newdata = newdata
                    , distances = seqx
                    , type = "dfuncs"
                    )
  
  # Numerical integration ----
  # Simpson's rule coefficients on f(x0), f(x1), ..., f(x(nEvalPts))
  # i.e., 1, 4, 2, 4, 2, ..., 2, 4, 1
  intCoefs <- rep( c(2,4), (nInts/2) ) # here we need nInts to be even
  intCoefs[1] <- 1
  intCoefs <- matrix(c(intCoefs, 1), ncol = 1)
  
  edr <- (t(y) %*% intCoefs) * dx / 3
  edr <- units::set_units(drop(edr), NULL) # convert from matrix to vector, drop units  
  edr <- sqrt( 2 * edr )  # cannot sqrt units (unless like m^2 are assigned)
  edr <- units::set_units(edr, object$outputUnits, mode = "standard") # add back units
  
    # OLD COMMENTS:
    # obj$detections$dist is in denominator of integration.constant for point surveys. 
    # multiply here to remove it. vector inside root should be constant.
    # rho <- sqrt(2 * integral * units::drop_units(obj$detections$dist))[1]
    
    # multiplying by obj$detections$dist in above line was a trick because we called 
    # integration.constant (because computations are complicated). But what about units? 
    # rho should have same units as obj$detections$dist, but one cannot take root of vectors with units. 
    # So, add back the units. 
    # rho <- units::set_units(rho, obj$outputUnits, mode = "standard")
  
  edr
}