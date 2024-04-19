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
#' @inherit halfnorm.like return seealso
#'    
#' @examples
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' negexp.like(log(0.01), d, covs)
#' 
#' # Changing slope parameter
#' plot(d, negexp.like(log(0.1), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, negexp.like(log(0.05), d, covs)$L.unscaled, col="blue")
#' 
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
  
  return( list(L.unscaled = key, 
               params = data.frame(par1 = beta))
  )  

}
