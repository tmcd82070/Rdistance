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
#' \emph{k} X \emph{p}, where 
#' \emph{k} = \code{nrow(a)}) is the number of coefficient
#' vectors to evaluate (cases) and \emph{p} 
#' = \code{ncol(a)})
#' is the number of covariate and expansion 
#' coefficients in the likelihood (i.e., rows are 
#' cases and columns are covariates). If \code{a} is a 
#' dimensionless vector, it is interpreted as a 
#' single row with \emph{k} = 1. 
#' Covariate coefficients in \code{a} are the first 
#' \emph{q} values (\emph{q} <= \emph{p}), and must 
#' be on a log scale.
#' 
#' @param dist A numeric vector of length \emph{n} or 
#' a single-column matrix (dimension \emph{n}X1) containing 
#' detection distances at which to evaluate the likelihood.
#' 
#' @param covars A numeric vector of length \emph{q} or a
#' matrix of dimension \emph{n}X\emph{q} containing 
#' covariate values 
#' associated with distances in argument \code{dist}. 
#' 
#' @param w.hi A numeric scalar containing maximum 
#' distance. The right-hand cutoff or upper limit. 
#' Ignored by some likelihoods (such as halfnorm, 
#' negexp, and hazrate), but is a fixed parameter
#' in other likelihoods (such as oneStep, heber, 
#' and uniform).
#' 
#' @details The half-normal distance function is 
#' \deqn{f(d|s) = \exp(-d^2 / (2*s^2))}{f(d|s) = exp(-d^2 / (2*s^2))}
#' where \eqn{s = exp(x'a)}, \eqn{x} is a vector of 
#' covariate values associated with distance \eqn{d} 
#' (i.e., a row of \code{covars}), and 
#' \eqn{a} is a vector of the first $q$ (=\code{ncol(covars)}) 
#' values in argument \code{a}.
#' 
#' Some authors parameterize the halfnorm without 
#' the "2" in the denominator of the exponent. 
#' \code{Rdistance} includes 
#' "2" in this denominator to make 
#' quantiles of the half normal agree with 
#' the standard normal. This means that half-normal 
#' coefficients in 
#' Rdistance (i.e., \emph{s = exp(x'a)}) can be 
#' interpreted as normal standard errors.  
#' Approximately 95\% of distances should 
#' occur between 0 and 2\emph{s}.
#' 
#' @return A list containing the following two components:
#' \itemize{
#'   \item \bold{L.unscaled}: A matrix of size 
#'    \emph{n}X\emph{k}X\emph{b} 
#'    containing likelihood values evaluated at 
#'    distances in \code{dist}.
#'    Each row is associated with 
#'    a single distance, and each column is associated with 
#'    a single case (row of \code{a}).  This matrix is  
#'    "unscaled" because the underlying likelihood does 
#'    not integrate to one. Values in \code{L.unscaled} 
#'    are always greater than or equal to zero.
#'    
#'  \item \bold{params}: A \emph{n}X\emph{k}X\emph{b} array 
#'  of the 
#'  likelihood's (canonical) parameters. First page contains 
#'  parameter values related to covariates (i.e., 
#'  \emph{s = exp(x'a)}),
#'  while subsequent pages contain other parameters. 
#'  \emph{b} = 1 for halfnorm, negexp; \emph{b} = 2 for 
#'  hazrate and others.
#'  Rows correspond to distances in \code{dist}. Columns 
#'  correspond to rows from argument \code{a}. 
#' }
#'  
#' @seealso \code{\link{dfuncEstim}}, 
#'          \code{\link{abundEstim}},
#'          other \code{<likelihood>.like} functions
#'          
#' @examples  
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' halfnorm.like(log(20), d, covs)
#' 
#' plot(d, halfnorm.like(log(20), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, halfnorm.like(log(40), d, covs)$L.unscaled, col="blue")
#' 
#' # Matrix inputs:
#' d <- matrix(c(0,10,20), ncol = 1) # 3X1
#' covs <- matrix(c(rep(1,nrow(d)), rep(.5,nrow(d))), nrow = nrow(d)) # 3X2
#' coefs <- matrix(log(c(15,5,10,10)), nrow=2) # 2X2
#' L <- halfnorm.like( coefs, d, covs ) 
#' L$L.unscaled # 3X2
#' L$params     # 3X2; exp(log(15)+0.5log(10)) and exp(log(5)+0.5log(10))
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
  
  # cat(crayon::red("In Halfnorm.like...\n"))
  q <- nCovars(covars)
  if(is.matrix(a)){
    # cat(crayon::red("A is matrix\n"))
    beta <- a[,1:q, drop = FALSE]  # k X q
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  sigma <- exp(s)  # link function here

  # print(dim(sigma))
  
  # Dropping units of dist is save b/c checked already
  # 'key' is unit-less
  dist <- units::set_units(dist, NULL)
  # dist <- matrix(dist
  #           , nrow = length(dist)
  #           , ncol = ncol(sigma)
  #           ) 
  # or, alternative dist <- matrix(dist,ncol=1) %*% matrix(1,1,length(dist))
  # cat("length(dist) = \n")
  # print(length(dist))
  # print(dist[1:5])
  key <- -(dist*dist)  # vector length n
  key <- key / (2*sigma*sigma)  # (n vector) / nXk 
  key <- exp(key)  # exp of density function here, not link.

  return( list(L.unscaled = key, 
               params = s))  # return params on log scale
    
  
}
