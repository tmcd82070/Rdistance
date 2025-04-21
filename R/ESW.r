#' @title Effective Strip Width (ESW) for line transects
#'   
#' @description Returns effective strip width (ESW) for 
#'   line-transect detection functions. 
#'   See \code{\link{EDR}} is for point transects.  
#'   
#' @inheritParams effectiveDistance
#' 
#' @details ESW is area under 
#'   a scaled distance function between its
#'   left-truncation limit (\code{obj$w.lo}) and its right-truncation 
#'   limit (\code{obj$w.hi}). \if{latex}{I.e., 
#'     \deqn{ESW = \int_{w.lo}^{w.hi} g(x)dx,} 
#'   where \eqn{g(x)} is the distance
#'   function scaled so that \eqn{g(x.scl) = g.x.scl}
#'   and \eqn{w.lo} and \eqn{w.hi} are the lower
#'   and upper truncation limits.  }
#'   
#'   If detection does not decline with distance, 
#'   the detection function is flat (horizontal), and 
#'   area under the detection  
#'   function is  \eqn{g(0)(w.hi - w.lo)}.  
#'   If, in this case, \eqn{g(0) = 1}, effective sampling distance is 
#'   the half-width of the surveys, \eqn{(w.hi - w.lo)}
#'   
#' @inherit effectiveDistance return   
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{EDR}}, 
#' \code{\link{effectiveDistance}}
#' 
#' @examples
#' data(sparrowDf)
#' dfunc <- sparrowDf |> dfuncEstim(formula=dist~bare)
#' 
#' ESW(dfunc) # vector length 356 = number of groups
#' ESW(dfunc, newdata = data.frame(bare = c(30,40))) # vector length 2
#' 
#' @keywords modeling
#' @importFrom stats predict
#' @export

ESW <- function( object, newdata = NULL ){
  
  # Issue error if the input detection function was fit to point-transect data

  if( Rdistance::is.points(object) ){
    stop("ESW is for line transects only.  See EDR for the point-transect equivalent.")
  } 

  likExpan <- paste0(object$likelihood, "_", object$expansions)
  
  esw <- switch(likExpan
            , "halfnorm_0" = integrateHalfnorm(object, newdata)
            , "negexp_0" = integrateNegexp(object, newdata)
            , "oneStep_0" = integrateOneStep(object, newdata)
            , integrateNumeric(object, newdata)
  )
  
  esw
  
}
