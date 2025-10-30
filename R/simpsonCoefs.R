#' @title Simpson numerical integration coefficients
#' 
#' @description 
#' Return a vector of Simpson's Composite numerical integration 
#' coefficients. 
#' 
#' @param n Number of coefficients, which is the number of points 
#' at which the function of interest is evaluated.  The number of 
#' intervals is \code{(n-1)/2}.  This number must be odd.
#' 
#' @return A vector of Simpson Composite rule coefficients 
#' suitable for numeric integration. The return is a vector of 
#' integers alternating between 4 and 2, with 1's on each end.
#' 
#' @details
#' Let \code{x} be an vector of equally spaced points in the domain 
#' of a function f (equally spaced is critical). 
#' Let \code{y = f(x)}. The numeric integral of f from \code{min(x)} 
#' to \code{max(x)} is 
#' \code{sum(simpsonCoefs(length(y)) * y) * (x[2] - x[1]) / 3}. 
#' 
#' @examples
#' 
#' x <- seq(0, 9, length=13)
#' y <- x^2
#' 
#' scoefs <- simpsonCoefs(length(x))
#' 
#' # exact integral is 9^3/3 = 243
#' sum( scoefs*y ) * (x[2] - x[1]) / 3
#' 
#' @export
simpsonCoefs <- function( n ){
  
  if( (n %% 2) == 0 ){
    stop(paste("Number of Simpson coefficients must be odd. Found", n))
  }
  intCoefs <- c(rep( c(2,4), ((n-1)/2) ), 1) 
  intCoefs[1] <- 1
  
  intCoefs
}