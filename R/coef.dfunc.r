#' @title coef.dfunc - Coefficients of an estimated detection function
#' 
#' @description Extract distance model coefficients from 
#' an estimated detection function object.
#'   
#' @usage \method{coef}{dfunc}(object, \dots)
#' 
#' @inheritParams is.smoothed
#' 
#' @return The estimated coefficient vector for the detection function. 
#' Length and interpretation of values vary 
#' depending on the form of the detection function and expansion terms.
#'   
#' @seealso \code{\link{AIC}}, \code{\link{dfuncEstim}}
#' 
#' @examples
#' sparrowDF <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' dfunc <- sparrowDf |> dfuncEstim(dist~bare + observer,
#'                       w.hi=units::set_units(150, "m"))
#' 
#' # Extract the coefficient(s)
#' coef(dfunc)
#' 
#' @keywords model
#' @export

coef.dfunc <- function(x, ...){
  if( x$likelihood == "smu" ){
    # smoothed distance function
    COEF <- NULL
  } else {
    COEF <- x$par
  }
  COEF
}
