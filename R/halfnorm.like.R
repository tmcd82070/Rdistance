#' @title halfnorm.like - Half-normal distance function
#' 
#' @description Evaluates the half-normal distance function, 
#' here called a likelihood, for 
#' sighting distances based on covariates.
#' 
#' @param a A vector of the likelihood parameter values. Length 
#' should be (number of covariates, including intercept) + 
#' (number of expansions). These should all be on a log scale
#' because link function used is exp. Covariates are related to
#' standard deviation of the halfnormal distance function. 
#' 
#' @param dist A numeric vector containing 
#' the observed distances.
#' 
#' @param covars A data frame or matrix containing values 
#' of the covariates for each distance observation. Number of 
#' columns must match number of parameters (i.e., length of \code{a}).
#' 
#' 
#' @details The half-normal distance function is 
#' \deqn{f(d|s) = \exp(-d^2 / (2*s^2))}{f(d|s) = exp(-d^2 / (2*s^2))}
#' where \eqn{s = exp(x'a)}, \eqn{x} is a column vector of 
#' covariate values associated with distance \eqn{d}, and 
#' \eqn{a} is a vector of the first \code{ncol(covars)} 
#' values in the input vector of parameters \code{a}.
#' 
#' Some authors  
#' do not use a "2" in the 
#' denominator of the exponent when defining a half-normal 
#' distance functions.  \code{Rdistance} uses a 
#' "2" in the denominator of the exponent to make 
#' quantiles of this function agree with 
#' the standard normal. This means \emph{s = exp(x'a)} can be 
#' interpreted as a normal standard error.  Hence, for example, 
#' approximately 95\% of distances should 
#' occur between 0 and 2\emph{s}.
#' 
#'   
#' @return A list containing two numeric vectors. One vector, 
#' named \code{$key}, is the value of the likelihood at every
#' input distance.  The second vector, named \code{params},
#' is a vector of likelihood parameters (i.e., \emph{s} in 
#' equations of Details section) for every input distance. 
#' Assuming \code{L} is the list returned by this function, 
#' the negative log likelihood is \code{-sum(log(L$key), na.rm=T)}. 
#' Note that returned likelihood values for distances less 
#' than \code{w.lo} or greater than \code{w.hi} are \code{NA}; 
#' hence, \code{na.rm=TRUE} in the sum. 
#' Values in the return are always greater than or equal to zero.
#'  
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples  \dontrun{
#' set.seed(238642)
#' x <- seq(0, 100, length=100)
#' ones <- matrix(1, nrow = length(x), ncol = 1)
#' 
#' halfnorm.like(log(20), x, ones)
#' 
#' # Plots showing effects of changes in parameter Sigma
#' plot(x, halfnorm.like(log(20), x, ones)$key, type="l", col="red")
#' lines(x, halfnorm.like(log(40), x, ones)$key, col="blue")
#' 
#' # Estimate 'halfnorm' distance function
#' a <- 5
#' x <- rnorm(1000, mean=0, sd=a)
#' x <- x[x >= 0]
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc)
#' 
#' @keywords models
#' @export

halfnorm.like <- function(a
                          , dist
                          , covars ){

  q <- ncol(covars)
  beta <- a[1:q] # could be expansion coefs after q
  s <- drop( covars %*% matrix(beta,ncol=1) )
  sigma <- exp(s)  # link function here

  if(inherits(dist, "units")){
    dist <- units::drop_units(dist)
  }
  key <- -(dist*dist)/(2*sigma*sigma)  
  # Above is safe. Units of sigma will scale to units of dist. 
  # 'key' is unit-less
  key <- exp(key)  # exp of density function here, not link.

  return( list(key = key, params = sigma))  

}
