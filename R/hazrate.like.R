#' @title hazrate.like - Hazard rate likelihood 
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
#' @keywords models
#' @export

hazrate.like <- function(a, 
                         dist, 
                         covars ){
  
  
  # What's in a? : 
  #   a = [(Intercept), b1, ..., bp, k, <expansion coef>]
  q <- Rdistance:::nCovars(covars)
  beta <- matrix(a[1:q], ncol = 1) # could be expansion coefs after q
  s <- drop(covars %*% beta)
  sigma <- exp(s)  # link function here
  K <- a[q + 1]

  dist <- units::set_units(dist, NULL)
  key <- -( dist/sigma )^(-K)
  key <- 1 - exp(key)

  return( list(L.unscaled = key, 
               params = data.frame(par1 = sigma,
                                   par2 = K))
          )  

}
