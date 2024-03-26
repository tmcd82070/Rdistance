#' @title huber.like - Huber distance function
#' 
#' @description 
#' Computes the Huber likelihood for use as a distance function. 
#' The Huber likelihood is based on Huber loss.  
#' It is quadratic from the lowest distance value to 
#' its first parameter, then linear from the first parameter
#' to its second. The function is zero after the second parameter. 
#' Values are scaled such that the integral from lowest distance 
#' value to the second parameter (\code{range}) is 1.0.
#' 
#' @param a Distance at which the likelihood 
#' transitions from quadratic to linear.  First derivative
#' is constant just before and after this point. 
#' 
#' @param range Distance beyond which the likelihood is zero.
#' 
#' @return Vector of huber likelihood values.
#' 
#' @examples
#' 
#' x <- 0:100
#' y <- huber.like(c(40, 80), x)
#' plot(x,y,type="l")
#' points(40,y[x == 40])
#' points(80, 0)
#' 
#' # It is a proper density
#' integrate( function(x, a){huber.like(a,x)}
#'    , from = 0
#'    , to = 100
#'    , a = c(40, 80)
#'    )
#' 
#' @export
#' 

huber.like <- function(a, dist, ml){
  
  # Restrictions : 0 < a <= range <= w.hi
  # dist must be >= 0, or this does not work right
  
  # need to somehow add covariates here. 
  
  X <- stats::model.matrix(ml$mt, ml$mf)
  
  # etc. The rest of this code is for case without covars
  
  h <- ifelse( dist <= a
              , 0.5 * dist^2
              , a*(dist - 0.5*a)
  )
  h <- a*(range - 0.5*a) - h
  h <- ifelse( h <= 0.0
              , 0.0
              , h
              )
  
  # Integrate under the function and scale
  integral0a <- a^2*range - (2/3)*a^3
  integralar <-  0.5*a*(range^2 + a^2 - 2*range*a)
  areaUnder <- integral0a + integralar
  
  h <- h / areaUnder
  
  h
  
}
