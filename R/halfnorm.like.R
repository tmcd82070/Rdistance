#' @title Half-normal distance function
#' 
#' @description Evaluate the half-normal distance function, 
#' for sighting distances, potentially including covariates
#' and expansion terms
#'
#'
#' @param a A vector or matrix of covariate 
#' and expansion term 
#' coefficients. If matrix, dimension is 
#' *k* X *p*, where 
#' *k* = `nrow(a)`) is the number of coefficient
#' vectors to evaluate (cases) and *p* 
#' = `ncol(a)`)
#' is the number of covariate and expansion 
#' coefficients in the likelihood (i.e., rows are 
#' cases and columns are covariates). If `a` is a 
#' dimensionless vector, it is interpreted as a 
#' single row with *k* = 1. 
#' Covariate coefficients in `a` are the first 
#' *q* values (*q* <= *p*), and must 
#' be on a log scale.
#' 
#' @param dist A numeric vector of length *n* or 
#' a single-column matrix (dimension *n*X1) containing 
#' detection distances at which to evaluate the likelihood.
#' 
#' @param covars A numeric vector of length *q* or a
#' matrix of dimension *n*X*q* containing 
#' covariate values 
#' associated with distances in argument `dist`. 
#' 
#' @param w.hi A numeric scalar containing maximum 
#' distance. The right-hand cutoff or upper limit. 
#' Ignored by some likelihoods (such as halfnorm, 
#' negexp, and hazrate), but is a fixed parameter
#' in other likelihoods (such as oneStep 
#' and uniform).
#' 
#' @details The half-normal distance function is 
#' \deqn{f(d|\sigma) = \exp(-\frac{d^2}{2\sigma^2})}{f(d|s) = exp(-d^2 / (2*s^2))}
#' where \eqn{\sigma = exp(x'a)}{s = exp(x'a)}, \eqn{x} is a vector of 
#' covariate values associated with distance \eqn{d} 
#' (i.e., a row of `covars`), and 
#' \eqn{a} is a vector of the first $q$ (=`ncol(covars)`) 
#' values in argument `a`.
#' 
#' Some authors parameterize the halfnorm without 
#' the "2" in the denominator of the exponent. 
#' `Rdistance` includes 
#' "2" in this denominator to make 
#' quantiles of the half normal agree with 
#' the standard normal. This means that half-normal 
#' coefficients in 
#' Rdistance (i.e., \eqn{\sigma = exp(x'a)}{s = exp(x'a)}) can be 
#' interpreted as normal standard errors.  
#' Approximately 95% of distances should 
#' occur between 0 and 2\eqn{\sigma}{s}.
#' 
#' @return A list containing the following two components:
#' \itemize{
#'   \item **L.unscaled**: A matrix of size 
#'    *n*X*k* (*n* = length `dist`; *k* = number of
#'    cases = `nrow(a)`)
#'    containing likelihood values evaluated at 
#'    distances in `dist`.
#'    Each row is associated with 
#'    a single distance, and each column is associated with 
#'    a single case (row of `a`).  Values in this matrix are  
#'    the distance function \eqn{g(d)} which generally have \eqn{g(0) = 1}.
#'    These values are "unscaled" likelihood values; they must be 
#'    scaled (divided by) with the area under \eqn{g(x)} between w.lo and w.hi  
#'    to form proper likelihood values. 
#'    
#'  \item **params**: A *n*X*b*X*k*  array 
#'  of the likelihood's (canonical) parameters in link space (i.e., on 
#'  log scale; *b* = number of canonical parameters in 
#'  the likelihood; *k* = number of cases). 
#'  Rows correspond to distances in `dist`. Columns 
#'  correspond to parameters (columns of `a`), 
#'  and pages correspond to cases (rows of `a`). 
#' }
#'  
#' @seealso [dfuncEstim()], 
#'          [abundEstim()],
#'          other `<likelihood>.like` functions
#'          
#' @examples  
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' halfnorm.like(log(20), d, covs)
#' 
#' plot(d, halfnorm.like(log(20), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, halfnorm.like(log(40), d, covs)$L.unscaled, col="blue")
#' 
#' # Evaluate 3 functions at once using matrix of coefficients:
#' # sigma ~ 20, 30, 40
#' coefs <- matrix(log(c(7.39,7.33, 4.48,44.80, 2.72,216.54))
#'                , byrow = TRUE
#'                , ncol=2) # (3 coef vectors)X(2 covars)
#' covs <- matrix(c(rep(1,length(d))
#'                , rep(0.5,length(d)))
#'                , nrow = length(d)) # 100 X 2
#' L <- halfnorm.like( coefs, d, covs ) 
#' L$L.unscaled # 100 X (3 coef vectors)
#' L$params     # 100 X (3 coef vectors); ~ log(c(20,30,40))
#' matplot(d, L$L.unscaled, type="l")
#' 
#' @export

halfnorm.like <- function(a
                          , dist
                          , covars
                          , w.hi = NULL
                          ){

  # w.hi is ignored, but needed for compatability in other likelihoods
  # cat(paste("In", crayon::red("halfnorm.like"), "\n"))
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
            "Found array with", length(dim(dist)), "dimensions."))
  }
  
  q <- nCovars(covars)
  if(is.matrix(a)){
    # cat(crayon::red("A is matrix\n"))
    beta <- a[,1:q, drop = FALSE]  # k X q
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  sigma <- exp(s)  # link function here

  dist <- dropUnits(dist)
  key <- drop(-(dist*dist))  # n-vector; use drop() in case dist is matrix
  key <- key / (2*sigma*sigma)  # (n vector) / nXk 
  key <- exp(key)  # exp of density function here, not link.
  
  # Rules for likelihoods:
  #  1. 'key' must be a matrix (not vector), dim(key) should = (length(dist), nrow(a))
  #  2. 'key' must be unscaled. It should not sum to 1. Max should be 1. 
  #     i.e., this is g(x) [not f(x)], or else ESW calculations are wrong.
  #  3. 'key' cannot have units.

  return( list(L.unscaled = key, # MUST be a MATRIX (not vector)
               params = s))  # return params on log scale
    
  
}
