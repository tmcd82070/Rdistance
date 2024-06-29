#' @title uniform.like - Uniform distance likelihood
#' 
#' @description Compute the uniform  
#' distance functions.   
#' 
#' @inheritParams halfnorm.like
#' 
#' @inherit halfnorm.like return seealso
#' 
#' @details The uniform (or constant) likelihood 
#' is, 
#' \deqn{f(d|a) = 1}
#' for \eqn{0 \leq d \leq a}{0 <= d <= a}, and 0 for all
#' \eqn{d}{d} less than 0 or greater than \eqn{a}{a}.  
#' Covariates effect parameter \eqn{a}{a} 
#' via the log link function, i.e., \eqn{a = exp(x'b)},
#' where \eqn{x} is the vector of covariate values 
#' associated with distance \eqn{d} and \eqn{b}
#' is the vector of estimated coefficients. Area under
#' the likelihood is \eqn{a}.
#' 
#' @examples
#' 
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' uniform.like( log(80), d, covs )
#' 
#' plot(d, uniform.like( log(80), d, covs)$L.unscaled, type = "l", col = "blue")
#' 
#' # Likelihood profile for sparrow data
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' dist <- tidyr::unnest(sparrowDf, cols = detections)$dist
#' covs <- matrix(1,length(dist),1)
#' a <- seq(75, 210, length= 100)
#' L <- rep(NA, length(a))
#' for(i in 1:length(a)){
#'   y <- uniform.like(log(a[i]), dist, covs)$L.unscaled / a[i]
#'   y[ !is.na(y) & y <= 0] <- .Machine$double.eps
#'   L[i] <- -sum(log(y), na.rm = TRUE) 
#' }
#' plot(a,L)
#' abline(v = 146)
#' 
#' @export
#' 
uniform.like <- function(a
                         , dist
                         , covars 
                         ){
  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, <expansion coef>]
  
  stop("Uniform likelihood is not ready for prime time. Use Logistic or hazrate.")
  
  q <- nCovars(covars)
  
  beta <- matrix(a[1:q], ncol = 1) 
  s <- drop( covars %*% beta )      
  beta <- exp(s)  # link function here
  
  d <- units::set_units(dist, NULL)
  
  key <- (1 / beta) * (d <= beta) + (1 / (max(d) - beta)) * (beta < d )

  return( list(L.unscaled = key, 
               params = data.frame(par1 = beta))
  )  
  
}  
  
  
  