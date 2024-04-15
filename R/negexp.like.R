#' @title negexp.like - Negative exponential likelihood
#' 
#' @description Computes the negative exponential distance function. 
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details The negative exponential likelihood is 
#' \deqn{f(x|a) = \exp(-ax)}{f(x|a) = exp( -a*x )} where \eqn{a} is the 
#' slope parameter to be estimated. 
#' 
#' @inheritSection halfnorm.like return
#'    
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples \dontrun{
#' set.seed(238642)
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in parameter Beta
#' plot(x, negexp.like(0.01, x), type="l", col="red")
#' plot(x, negexp.like(0.05, x), type="l", col="blue")
#' 
#' # Estimate 'negexp' distance function
#' Beta <- 0.01
#' x <- rexp(1000, rate=Beta)
#' dfunc <- dfuncEstim(x~1, likelihood="negexp")
#' plot(dfunc)
#' }
#'          
#' @keywords models
#' @export

negexp.like <- function (a, 
                         dist, 
                         covars){

  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, <expansion coef>]
  
  q <- Rdistance:::nCovars(covars)
  
  beta <- matrix(a[1:q], ncol = 1) 
  s <- drop( covars %*% beta )      
  beta <- exp(s)  # link function here
  
  key = -beta * units::set_units(dist, NULL)
  key <- exp(key)

  return( list(key = key, params = beta))  
  
}
