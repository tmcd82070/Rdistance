#' @title Mixture of two uniforms likelihood
#' 
#' @description
#' Compute likelihood function for a mixture of two uniform
#' distributions. 
#' 
#' @inheritParams halfnorm.like
#' 
#' @inherit halfnorm.like return seealso
#' 
#' @details Rdistance's \code{oneStep} likelihood is a mixture of two 
#' non-overlapping uniform distributions. The 'oneStep' density function
#' is  
#' \deqn{f(d|p, \theta) = \frac{p}{\theta}I(0 <= d <= \theta) + 
#'        \frac{1 - p}{w - \theta}I(\theta < d <= w),}
#' where \eqn{I(x)} is the indicator function for event \eqn{x}, 
#' and \eqn{w} is the nominal strip width (i.e., \code{w.hi} in Rdistance). 
#' \eqn{w} is fixed - given by the user. 
#' The unknown parameters to be estimated 
#' are \eqn{\theta}{T} and \eqn{p}{p}.
#'  
#' Covariates influence values of \eqn{\theta}{T} 
#' via a log link function, i.e., \eqn{\theta = exp(x'b)}{T = exp(x'b)},
#' where \eqn{x} is the vector of covariate values 
#' associated with distance \eqn{d}, and \eqn{b}
#' is the vector of estimated coefficients. 
#' 
#' 
#' 
#' 
#' @references  
#' Peter F. Craigmile & D.M. Tirrerington (1997) "Parameter estimation for 
#' finite mixtures of uniform distributions", 
#' Communications in Statistics - Theory and Methods, 26:8, 1981-1995, 
#' DOI: 10.1080/03610929708832026
#' 
#'  A. Hussein & J. Liu (2009) "Parametric estimation of mixtures of two 
#'  uniform distributions", Journal of Statistical Computation and Simulation, 
#'  79:4, 395-410, DOI:10.1080/00949650701810406
#'  
#' @examples
#' 
#' # Fit oneStep to simulated data
#' whi <- 250
#' T <- 100
#' p <- 0.85
#' n <- 200 
#' x <- c( runif(n*p, min=0, max=T), runif(n*(1-p), min=T, max=whi))
#' x <- units::set_units(x, "m")
#' tranID <- sample(rep(1:10, each=n/10), replace=FALSE)
#' detectDf <- data.frame(transect = tranID, dist = x)
#' siteDf <- data.frame(transect = 1:10
#'                , length = rep(units::set_units(10,"m"), 10))
#' distDf <- RdistDf(siteDf, detectDf)
#' 
#' # Estimation
#' fit <- dfuncEstim(distDf
#'                  , formula = dist ~ 1
#'                  , likelihood = "oneStep"
#'                  , w.hi = units::set_units(whi, "m")
#'                  )
#' plot(fit)
#' thetaHat <- exp(coef(fit)[1]) 
#' pHat <- coef(fit)[2]
#' c(thetaHat, pHat) # should be close to c(100,0.85)
#'
#' summary(abundEstim(fit, ci=NULL)) 
#' 
#' @export


oneStep.like <- function(a
                , dist
                , covars 
                , w.hi = NULL) {
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  q <- nCovars(covars)
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
    p <- a[, q+1, drop = FALSE]     # k X 1
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    p <- matrix(a[q+1], nrow = 1)     # 1 X 1
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  theta <- exp(s)  # link function here
  
  # Dropping units of dist is safe b/c checked already
  # 'key' is unit-less
  dist <- units::set_units(dist, NULL)
  dist <- matrix(dist
                 , nrow = length(dist)
                 , ncol = ncol(theta)
  ) 
  
  if(is.null(w.hi)){
    w.hi <- max(dist)  # no units b/c removed above
  } else {
    w.hi <- units::set_units(w.hi, NULL) # already checked units
  }
  
  # or, alternative dist <- matrix(dist,ncol=1) %*% matrix(1,1,length(dist))
  p <- matrix(p, nrow = nrow(theta), ncol = ncol(theta))
  key <- (0 <= dist & dist <= theta) + 
         (((1-p) * theta) / ((w.hi - theta) * p)) * (theta < dist & dist <= w.hi)

  return( list(L.unscaled = key, 
               params = theta))

}