#' @title Gamma distance function 
#' 
#' @description Evaluate the gamma distance function 
#' for sighting distances, potentially including covariates
#' and expansion terms
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details 
#' 
#' The Rdistance implementation of a gamma distance function follows 
#' Becker and Quang (2009). Rdistance's gamma distance function is 
#' \deqn{f(d|\alpha, \sigma) = \frac{1}{\sigma^\alpha \Gamma(\alpha)}d^{\alpha - 1}e^{-d/\sigma}}{
#' f(d|a,s) = (s^a Gamma(a))^{-1} d^{a-1} exp(-d/s)
#' }
#' where the scale parameter \eqn{\sigma = k [exp(x'\beta)]}{s = k*exp(x'b)},
#' \deqn{k = \frac{1}{\Gamma(\alpha)}  \left(\frac{a - 1}{e^1} \right)^{a - 1}}{
#' k = (1/Gamma(a)) * (((a - 1)/exp(1))^(a - 1)),}  
#' \eqn{x} is a vector of covariate values associated with distance \eqn{d} 
#' (i.e., a row of \code{covars}), and \eqn{\beta}{b} is a vector of the 
#' first \eqn{q} (=\code{ncol(covars)}) values of the first argument 
#' of the function (\code{a}). The shape parameter \eqn{\alpha}{a} is the 
#' \eqn{q+1}-st value in the function's first argument. 
#'  
#'  
#' Rdistance uses R's \code{dgamma} function to evaluate 
#' the gamma density function. The call is \code{dgamma(d, shape = alpha, scale = k*s )}
#' where \code{alpha} = \code{a[q+1]}, \code{k = (1/gamma(alpha)) * (((alpha - 1)/exp(1))^(alpha - 1))}, 
#' and \code{s = exp(x*a[1:q])} (\code{q} = \code{ncol(covars)}). 
#'   
#' @inherit halfnorm.like return seealso
#'    
#' @references Becker, E. F., and P. X. Quang, 2009. \emph{A Gamma-Shaped Detection Function for Line-Transect Surveys with Mark-Recapture and Covariate Data.}
#'   Journal of Agricultural, Biological, and Environmental Statistics 14(2):207-223.
#'   
#' @examples 
#' x <- seq(0, 100, length=100)
#' covars <- matrix(1,100,1)
#' 
#' # Plots showing changes in scale
#' plot(x, Gamma.like(c(log(20),2.5), x, covars)$L.unscaled, type="l", col="red")
#' lines(x, Gamma.like(c(log(40),2.5), x, covars)$L.unscaled, col="blue")
#' 
#' # Plots showing changes in shape
#' plot(x, Gamma.like(c(log(20),1.5), x, covars)$L.unscaled, type="l", col="red")
#' lines(x, Gamma.like(c(log(20),2.5), x, covars)$L.unscaled, col="blue")
#' lines(x, Gamma.like(c(log(20),4.5), x, covars)$L.unscaled, col="green")
#' 
#' @export
Gamma.like <- function(a
                       , dist
                       , covars
                       , w.hi = NULL
                       ){

  # What's in a? : 
  #   If no covariates: a = [Shape, Scale, <expansion coef>]
  #   If covariates:    a = [(Intercept), b1, ..., bp, Shape, <expansion coef>]
  
  # w.hi is ignored, but needed for compatability in other likelihoods
  # cat(paste("In", crayon::red("gamma.like"), "\n"))
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  
  q <- nCovars(covars)
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
    shp <- a[1, q+1, drop = TRUE]     # 1 X 1
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    shp <- a[q+1]     # 1 X 1
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  scl <- exp(s)  # link function here
  
  dist <- dropUnits(dist)

  dgamPars <- GammaReparam(shp, scl)
  key <- stats::dgamma( dist, shape=dgamPars$shp, scale=dgamPars$scl )
  
  # Scale like to have max 1
  m <- (dgamPars$shp - 1)*dgamPars$scl
  keyAtM <- stats::dgamma( m, shape=dgamPars$shp, scale=dgamPars$scl )
  
  key <- key / keyAtM
  
  # Note: Mode of Gamma distribution is (dgamPars$shp - 1)*dgamPars$scl,
  # or scl * b * (shp - 1) where b = (1/gamma(shp)) * (((shp - 1)/exp(1))^(shp - 1)) 

  return( list(L.unscaled = key, 
               params = cbind(s, shp)))  # return params on log scale

}
