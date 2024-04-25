#' @title ESW - Effective Strip Width (ESW) 
#'   
#' @description Returns effective strip width (ESW) for 
#'   line transect detection
#'   functions. 
#'   See \code{EDR} is for point transects.  
#'   
#' @inheritParams effectiveDistance
#' 
#' @details Effective strip width (ESW) is the
#'   integral of the distance function after scaling for g(0). 
#'   That is, ESW is the area under 
#'   the scaled distance function between its
#'   left-truncation limit (\code{obj$w.lo}) and its right-truncation 
#'   limit (\code{obj$w.hi}). \if{latex}{I.e., 
#'     \deqn{ESW = \int_{w.lo}^{w.hi} g(x)dx,} 
#'   where \eqn{g(x)} is the height of the distance
#'   function at distance \eqn{x}, and \eqn{w.lo} and \eqn{w.hi} are the lower
#'   and upper truncation limits used during the survey.  }
#'   
#'   If detection does not decline with distance, 
#'   the detection function is flat (horizontal), and 
#'   area under the detection  
#'   function is  \eqn{g(0)(w.hi - w.lo)}.  
#'   If \eqn{g(0) = 1}, effective sampling distance is 
#'   the half-width of the surveys, \eqn{(w.hi - w.lo)}
#'   
#' @section Numeric Integration: 
#' Rdistance uses Simpson's compsite 1/3 rule to numerically 
#' integrate under each distance function. 
#' Two hundred and one intervals are used in the integration.
#' In rare cases, this is not enought.  Users should 
#' modify this function's code and bump \code{seq.length} to 
#'   a value greater than 200.
#'   
#' @inherit effectiveDistance return   
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{EDR}}, 
#' \code{\link{effectiveDistance}}
#' 
#' @examples
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' dfunc <- sparrowDf |> dfuncEstim(formula=dist~1)
#' 
#' ESW(dfunc)
#' 
#' @keywords modeling
#' @export

ESW <- function( object, newdata ){
  
  # Issue error if the input detection function was fit to point-transect data

  if(is.points(object)){
    stop("ESW is for line transects only.  See EDR for the point-transect equivalent.")
  } 

  nEvalPts <- 201 # MUST BE ODD!!!
  nInts <- nEvalPts - 1 # this will be even if nEvalPts is odd
  seqx = seq(w.lo, w.hi, length=nEvalPts) 
  dx <- units::set_units(seqx[2] - seqx[1], NULL)  # or (w.hi - w.lo) / (nInts)

  y <- stats::predict(object = object
                     , newdata = newdata
                     , distances = seqx
                     , type = "dfuncs"
  )
  
  # Numerical integration ----
  # Use composite Simpson's 1/3 rule
  # Simpson's rule coefficients on f(x0), f(x1), ..., f(x(nEvalPts))
  # i.e., 1, 4, 2, 4, 2, ..., 2, 4, 1
  intCoefs <- rep( c(2,4), (nInts/2) ) # here we need nInts to be even
  intCoefs[1] <- 1
  intCoefs <- matrix(c(intCoefs, 1), ncol = 1)
  
  esw <- (y %*% intCoefs) * dx / 3
  
  # Trapazoid rule: Computation used in Rdistance version < 0.2.2
  # y1 <- y[,-1,drop=FALSE]
  # y  <- y[,-ncol(y),drop=FALSE]
  # esw <- dx*rowSums(y + y1)/2
  
  # Trapezoid rule. (dx/2)*(f(x1) + 2f(x_2) + ... + 2f(x_n-1) + f(n)) 
  # Faster than above, maybe.
  # ends <- c(1,nrow(y))
  # esw <- (dx/2) * (colSums( y[ends, ,drop=FALSE] ) + 
  #                  2*colSums(y[-ends, ,drop=FALSE] ))

  esw
  
}
