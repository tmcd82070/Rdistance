#' @title halfnorm.like - Half-normal distance function
#' 
#' @description Evaluates the half-normal distance function, 
#' here called a likelihood, for 
#' sighting distances based on covariates.
#' 
#' @param a A vector of covariate coefficients and other likelihood 
#' parameter values. Length 
#' should be (number of covariates including intercept) + 
#' (number of expansions) + (likelihood %in% c("hazrate","logistic"). 
#' Coefficients should all be on a log scale
#' because the link function for all likelihoods is exponential. 
#' 
#' @param dist A numeric vector containing 
#' the observed detection distances.
#' 
#' @param covars A data frame or matrix containing values 
#' of the covariates for each distance observation. 
#' 
#' @details The half-normal distance function is 
#' \deqn{f(d|s) = \exp(-d^2 / (2*s^2))}{f(d|s) = exp(-d^2 / (2*s^2))}
#' where \eqn{s = exp(x'a)}, \eqn{x} is a column vector of 
#' covariate values associated with distance \eqn{d}, and 
#' \eqn{a} is a vector of the first \code{ncol(covars)} 
#' values in the input vector of parameters \code{a}.
#' 
#' Some authors  do not place a "2" in the 
#' denominator of the exponent in the half-normal 
#' distance function.  \code{Rdistance} includes 
#' "2" in this denominator to make 
#' quantiles of the half normal agree with 
#' the standard normal. This means the predicted values 
#' from an Rdistance model (i.e., \emph{s = exp(x'a)}) can be 
#' interpreted as normal standard errors.  For example, 
#' approximately 95\% of distances should 
#' occur between 0 and 2\emph{s}.
#' 
#'   
#' @return A list containing two components. One component, 
#' named \code{L.unscaled}, is a vector of the likelihood 
#' values at every
#' observed distance. L is "unscaled" because it does 
#' not integrate to one. Scaling happens elsewhere.
#' 
#' The second component, named \code{params},
#' is a data frame of the likelihood's (canonical) parameters 
#' for every input distance. Columns in the data frame
#' are named \emph{par1}, \emph{par2}, ..., \emph{parN}. Size
#' of the data frame is number of observations by number 
#' of canonical parameters.
#' 
#' Assuming \code{L} is the list returned by this function, 
#' the negative log likelihood is \code{-sum(log(L$L.unscaled / I), na.rm=T)}, 
#' where \code{I} is the integration constant, or 
#' area under the likelihood between 
#' \code{w.lo} and \code{w.hi}. 
#' Note that returned likelihood values for distances less 
#' than \code{w.lo} or greater than \code{w.hi} are \code{NA}; 
#' hence, \code{na.rm=TRUE} in the sum. 
#' Values in \code{L$L.unscaled} are always greater 
#' than or equal to zero.
#'  
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}},
#'          \code{\link{triangle.like}}
#'          
#' @examples  
#' d <- seq(0, 100, length=100)
#' covs <- matrix(1,length(d),1)
#' halfnorm.like(log(20), d, covs)
#' 
#' plot(d, halfnorm.like(log(20), d, covs)$L.unscaled, type="l", col="red")
#' lines(d, halfnorm.like(log(40), d, covs)$L.unscaled, col="blue")
#' 
#' @keywords models
#' @export

halfnorm.like <- function(a
                          , dist
                          , covars
                          ){

  # cat(paste("In", crayon::red("halfnorm.like"), "\n"))
  
  # dist can be vector length n or nX1 matrix
  # covars can be 1 X q or n X q where n = length(dist)|nrow(dist)
  # a (coefficients) can be a vector length p (>=q) or matrix k X p
  #  where k = nrow(a) = number of coefficient vectors to evaluate
  # output is n X k
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector.",
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

  # Dropping units of dist is save b/c checked already
  # 'key' is unit-less
  dist <- units::set_units(dist, NULL)
  dist <- matrix(dist
            , nrow = length(dist)
            , ncol = ncol(sigma)
            ) 
  # or, alternative dist <- matrix(dist,ncol=1) %*% matrix(1,1,length(dist))
  key <- -(dist*dist)/(2*c(sigma*sigma))  
  key <- exp(key)  # exp of density function here, not link.

  return( list(L.unscaled = key, 
               params = sigma))
    
  
}
