#' @title Negative exponential likelihood
#' 
#' @description Computes the negative exponential distance function. 
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details The negative exponential likelihood is 
#' \deqn{f(x|a) = \exp(-ax)}{f(x|a) = exp( -a*x )} where \eqn{a} is the 
#' slope parameter. 
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

negexp.like <- function(a
                      , dist
                      , covars
                      , w.hi = NULL
                      ){

  # w.hi is ignored, but needed for compatability in other likelihoods
  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, <expansion coef>]
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  
  q <- nCovars(covars)
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  sigma <- exp(s)  # link function here

  dist <- units::set_units(dist, NULL)
  # dist <- matrix(dist
  #                , nrow = length(dist)
  #                , ncol = ncol(sigma)
  # ) 
  
  key = -sigma * dist  # (nXk) * (nXk)
  key <- exp(key)
  
  return( list(L.unscaled = key, 
               params = s)
  )  

}
