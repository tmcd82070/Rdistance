#' @title coef.dfunc - Coefficients of an estimated detection function
#' 
#' @description Extract the coefficients and estimated parameters (if any) from 
#'   a estimated detection function object.
#'   
#' @usage \method{coef}{dfunc}(object, \dots)
#' 
#' @param object An estimated distance function object.  An estimated distance 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}.
#' @param \dots Required for compatibility with the general \code{coef} method.  Any 
#'   extra arguments to this function are ignored.
#'   
#' @return The estimated parameter vector for the detection function. 
#' Length and interpretation of values in this vector vary 
#' depending on the form of the detection function and expansion terms.
#'   
#' @seealso \code{\link{AIC}}, \code{\link{dfuncEstim}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1
#'                   , detectionData=sparrowDetectionData)
#' 
#' # Print results
#' dfunc
#'   
#' # Extract the coefficient(s)
#' coef(dfunc)
#' 
#' @keywords model
#' @export

coef.dfunc <- function(object, ...){
  if( object$likelihood == "smu" ){
    # smoothed distance function
    COEF <- NULL
  } else {
    COEF <- object$par
  }
  COEF
}
