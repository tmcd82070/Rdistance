#' @title Hazard rate likelihood 
#' 
#' @description Computes the hazard rate distance function. 
#'
#' @inheritParams halfnorm.like 
#' 
#' @details 
#' The hazard rate likelihood is 
#' \deqn{f(x|\sigma,k) = 1 - \exp(-(x/\sigma)^{-k})}{%
#' f(x|Sigma,k) = 1 - exp(-(x/Sigma)^(-k))} 
#' where \eqn{\sigma}{Sigma} determines location 
#' (i.e., distance at which the function equals 1 - exp(-1) = 0.632), 
#' and \eqn{k}{k} determines slope of the function 
#' at \eqn{\sigma}{Sigma} (i.e., larger k equals steeper 
#' slope at \eqn{\sigma}{Sigma}). For distance analysis, 
#' the valid range for both \eqn{\sigma}{Sigma} and k is
#' \eqn{\geq 0}{>=0}.  
#'   
#'  
#' @inherit halfnorm.like return seealso
#'          
#' @examples
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' hazrate.like(c(log(20), 5), d, covs)
#' 
#' # Changing location parameter
#' plot(d, hazrate.like(c(log(20), 5), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, hazrate.like(c(log(40), 5), d, covs)$L.unscaled, col="blue")
#' abline(h = 1 - exp(-1), lty = 2)
#' abline(v = c(20,40), lty = 2)
#' 
#' # Changing slope parameter
#' plot(d, hazrate.like(c(log(50), 20), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, hazrate.like(c(log(50), 2), d, covs)$L.unscaled, col="blue")
#' abline(h = 1 - exp(-1), lty = 2)
#' abline(v = 50, lty = 2)
#' 
#'          
#' @export

hazrate.like <- function(a 
                       , dist 
                       , covars
                       , w.hi = NULL
                       ){

  # w.hi is ignored, but needed for compatability in other likelihoods
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  
  # What's in a? : 
  #   a = [(Intercept), b1, ..., bp, k, <expansion coef>]
  q <- nCovars(covars) # in Rdistance, not exported
  if(is.matrix(a)){
    beta <- a[, 1:q, drop = FALSE]  # k X q
    K <- a[, q+1, drop = FALSE]     # k X 1
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    K <- matrix(a[q+1], nrow = 1)     # 1 X 1
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  sigma <- exp(s)  # link function here

  dist <- units::set_units(dist, NULL)
  dist <- matrix(dist
                 , nrow = length(dist)
                 , ncol = ncol(sigma)
  ) 
  K <- matrix(K
            , nrow = nrow(dist)
            , ncol = ncol(sigma)
            , byrow = TRUE
            )
  key <- -( dist/sigma )^(-K)  # (nXk / nXk) ^ (kX1)
  key <- 1 - exp(key)

  sigma <- array(c(sigma, K)
               , dim = c(nrow(dist), ncol(sigma), 2))
  return( list(L.unscaled = key, 
               params = sigma)
          )  

}
