#' @title logistic.like - Logistic distance function likelihood
#' 
#' @description Computes a two parameter logistic distance function.
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details 
#' The 'logistic' likelihood contains two 
#' parameters.  Parameter \eqn{a} determines the scale and is 
#' labeled 'threshold' in Rdistance.  Parameter \eqn{b} determines 
#' sharpness (slope) of the likelihood's decrease at \eqn{a} and is labeled
#' 'knee' in Rdistance.  
#' (This function is called the 
#' \emph{heavy side} function in engineering).  The form 
#' of the function is, 
#' \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = 
#' \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}{%
#' f(x|a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-a))).} 
#' 
#' Parameter \eqn{a} is the location (distance) where the 
#' (unscaled) likelihood equals  
#' 0.5. That is, the inverse likelihood of 0.5 
#' is \eqn{a}, before scaling; or, 
#' \code{logistic.like( c(a,b), a, matrix(1,1,1))} equals 
#' \code{0.5}). 
#' 
#' Parameter \eqn{b} is slope of function 
#' at \eqn{a}.  
#' Prior to scaling, 
#' slope of the likelihood at \eqn{a} is \eqn{-b/4}. 
#' If \eqn{b}
#' is large, the "knee" is sharp and the likelihood can look 
#' uniform with support from 
#' \code{w.lo} to \eqn{a/f(0)}.  If \eqn{b} is small, the 
#' "knee" is shallow and the density of observations declines 
#' in an elongated "S" shape pivoting at \eqn{a/f(0)}.  
#' As \eqn{b} grows large and assuming f(0) = 1, the effective 
#' strip width approaches \eqn{a}.  
#' 
#' See plots in Examples. 
#'
#' @inherit halfnorm.like return seealso 
#'   
#'          
#' @examples 
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' logistic.like(c(log(20), 1), d, covs)
#' 
#' # Changing threshold parameter
#' plot(x, logistic.like(c(log(20), 20), x, covs)$L.unscaled, type="l", col="red")
#' lines(x, logistic.like(c(log(40), 20), x, covs)$L.unscaled, col="blue")
#' ablines(h = 0.5, lty = 2)
#' ablines(v = c(20,40), lty = 2)
#' 
#' # Changing knee parameter
#' plot(x, logistic.like(c(log(50), 100), x, covs)$L.unscaled, type="l", col="red")
#' lines(x, logistic.like(c(log(50), 1), x, covs)$L.unscaled, col="blue")
#' lines(x, logistic.like(c(log(50), .1), x, covs)$L.unscaled, col="orange")
#' 
#'          
#' @export
#'
logistic.like <- function(a
                        , dist
                        , covars){

  # rule is: parameter 'a' never has units.
  # upon entry: 'dist', 'w.lo', and 'w.hi' all have units 
  
  #   A couple internal functions first.
  #   This is the heavy-side function.  Basically, a steep logistic. f is just heavi flipped over
  heavi <- function(x,k){ 1 / (1 + exp( -k * x ))}
  f <- function(beta1, beta2, x){ 1 - heavi(x-beta1,beta2) }

  # What's in a? : 
  #   a = [(Intercept), b1, ..., bp, b, <expansion coef>]
  
  q <- Rdistance:::nCovars(covars)
  beta <- a[1:q]
  s <- drop( covars %*% beta )
  A.param <- exp(s)  # link function here
  B.param <- a[q+1]
  
  d <- units::set_units(dist, NULL)
  key <- f(A.param, B.param, d)

  return( list(L.unscaled = key, 
               params = data.frame(par1 = A.param,
                                   par2 = B.param))
          )  
  
}
