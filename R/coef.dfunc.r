#' @title coef.dfunc - Coefficients of an estimated detection function
#' 
#' @description Extract distance model coefficients from 
#' an estimated detection function object.
#'   
#' @usage \method{coef}{dfunc}(object, \dots)
#' 
#' @inheritParams is.smoothed
#' 
#' @param ... Ignored
#' 
#' @return The estimated coefficient vector for the detection function. 
#' Length and interpretation of values vary 
#' depending on the form of the detection function and expansion terms.
#'   
#' @seealso \code{\link{AIC}}, \code{\link{dfuncEstim}}
#' 
#' @examples
#' data(sparrowDfuncObserver) # pre-estimated dfunc
#' 
#' # Same as sparrowDfuncObserver$par 
#' coef(sparrowDfuncObserver) 
#' 
#' \dontrun{
#' data(sparrowDf)
#' dfunc <- sparrowDf |> dfuncEstim(dist~bare + observer,
#'                       w.hi=units::set_units(150, "m"))
#' coef(dfunc)
#' }
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
