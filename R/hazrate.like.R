#' @title hazrate.like - Hazard rate likelihood 
#' 
#' @description Computes the hazard rate distance function. 
#'
#' @inheritParams halfnorm.like 
#' 
#' @details The hazard rate likelihood is 
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
#' @inheritSection halfnorm.like return
#'  
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples \dontrun{
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in sigma
#' plot(x, hazrate.like(c(20, 5), x)$key, type="l", col="red")
#' plot(x, hazrate.like(c(40, 5), x)$key, type="l", col="blue")
#' 
#' # Plots showing effects of changes in beta
#' plot(x, hazrate.like(c(50, 20), x)$key, type="l", col="red")
#' plot(x, hazrate.like(c(50, 2), x)$key, type="l", col="blue")
#' }
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
  
  dist <- units::set_units(dist, NULL)
  K <- a[q + 1]
  key <- -( dist/sigma )^(-K)
  key <- 1 - exp(key)

  return( list(key = key, params = c(sigma, K)))  

}
