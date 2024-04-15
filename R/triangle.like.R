#' @title triangle.like - Triangular distance function
#' 
#' @description 
#' Computes the triangular likelihood for use as a distance function. 
#' The triangular likelihood has constant downward slope to a 
#' maximum sighting distance. 
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details For \eqn{x > 0}{x > 0}, the triangular likelihood 
#' is, 
#' \deqn{f(x|a) = 2a(1 - x/a)}{f(x|a) = 2*a*(1 - x/a)}
#' for all \eqn{0 <= x <= a}{0 <= x <= a}, and 0 for all
#' \eqn{x}{x} less than 0 and greater than \eqn{a}{a}.  
#' Parameter \eqn{a}{a} is related to covariates through 
#' a log link function.
#' 
#' 
#' @inheritSection halfnorm.like value
#'    
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{logistic.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples
#' 
#' x <- 0:100
#' X <- matrix(1,length(x), 1)
#' y <- triangle.like( log(80), x, X )$key
#' plot(x,y, type = "l")
#' 
#' @export
#' 
triangle.like <- function(a
                          , dist
                          , covars
                          ){
  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, <expansion coef>]
  
  q <- Rdistance:::nCovars(covars)
  
  beta <- matrix(a[1:q], ncol = 1) 
  s <- drop( covars %*% beta )      
  beta <- exp(s)  # link function here
  
  d <- units::set_units(dist, NULL)
  key <- 1 - (d / beta)
  key[ key < 0 ] <- 0

  return( list(key = key, params = beta))  
  
  # L <- (2/a)*(1 - dist/a)
  # L[ L < 0 ] <- 0
  # L
}