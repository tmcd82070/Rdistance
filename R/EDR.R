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

  likExpan <- paste0(object$likelihood, "_", object$expansions)
  
  edr <- switch(likExpan
                , "oneStep_0" = integrateOneStepPoints(object, newdata)
                , integrateNumeric(object, newdata)
  )
  
  edr
}