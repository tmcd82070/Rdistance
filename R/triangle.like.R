#' @title triangle.like - Triangular distance function
#' 
#' @description 
#' Computes the triangular likelihood for use as a distance function. 
#' The triangular likelihood has constant downward slope to a 
#' maximum sighting distance. 
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details The triangular likelihood 
#' is, 
#' \deqn{f(d|a) = 1 - \frac{d}{a}}{f(d|a) = 1 - d/a}
#' for \eqn{0 \leq d \leq a}{0 <= d <= a}, and 0 for all
#' \eqn{d}{d} less than 0 or greater than \eqn{a}{a}.  
#' Covariates effect parameter \eqn{a}{a} 
#' via the log link function, i.e., \eqn{a = exp(x'b)},
#' where \eqn{x} is the vector of covariate values 
#' associated with distance \eqn{d} and \eqn{b}
#' is the vector of estimated coefficients. Area under
#' the likelihood is \eqn{a/2}.
#' 
#' 
#' @inherit halfnorm.like return seealso
#'    
#'          
#' @examples
#' 
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' triangle.like( log(80), d, covs )
#' 
#' plot(d, triangle.like( log(80), d, covs)$L.unscaled, type = "l", col = "blue")
#' lines(d, triangle.like( log(60), d, covs)$L.unscaled, col = "red")
#' 
#' # Likelihood profile for sparrow data
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' dist <- tidyr::unnest(sparrowDf, cols = detections)$dist
#' covs <- matrix(1,length(dist),1)
#' a <- seq(75, 210, length= 100)
#' L <- rep(NA, length(a))
#' for(i in 1:length(a)){
#'   y <- triangle.like(log(a[i]), dist, covs)$L.unscaled * (2/a[i])
#'   y[ !is.na(y) & y <= 0] <- .Machine$double.eps
#'   L[i] <- -sum(log(y), na.rm = TRUE) 
#' }
#' plot(a,L)
#' abline(v = 146)
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

  return( list(L.unscaled = key, 
               params = data.frame(par1 = beta))
  )  

}