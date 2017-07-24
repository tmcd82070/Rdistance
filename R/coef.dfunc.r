#' @name coef.dfunc
#' @aliases coef.dfunc
#' @title Coefficients of an estimated detection function
#' @description Extract the coefficients and estimated parameters (if any) from 
#'   a estimated detection function object.
#' @usage \method{coef}{dfunc}(object, \dots)
#' @param object An estimated distance function object.  An estimated distance 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{F.dfunc.estim}.
#' @param \dots Required for compatability with the general \code{coef} method.  Any 
#'   extra arguments to this function are ignored.
#' @details This is an extractor function for the parameters of an estimated detection function. 
#'   This function is equivalent to \code{obj$parameters} for classical detection functions.
#' @return The estimated parameter vector for the detection function. Length and interpretation of values 
#'   in this vector vary depending on the form of the detection function and expansion terms.
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{AIC}}, \code{\link{F.dfunc.estim}}
#' @examples # Load the example dataset of sparrow detections from package
#'   data(sparrow.detections)
#'   
#'   # Fit detection function to perpendicular, off-transect distances
#'   dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)
#'   
#'   # Extract the coefficient(s)
#'   coef(dfunc)
#' @keywords model

coef.dfunc <- function(object, ...){
object$parameters
}
