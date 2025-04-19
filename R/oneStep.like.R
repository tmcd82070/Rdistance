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
#' \deqn{f(d|p, \theta) = pU(d;0,\theta) + (1-p)U(d;\theta,w)}
#' where U(x;a,b) is a regular uniform distribution on the interval 
#' a to b, and w is the nominal strip width (i.e., \code{w.hi} in Rdistance). 
#' Specifically, the 'oneStep' density is
#' \deqn{f(d|p, \theta) = \frac{p}{\theta}I(0 <= d <= \theta) + 
#'        \frac{1 - p}{w - \theta}I(\theta <= d <= w)}.
#' The unknown parameters are \eqn{\theta}{T} and \eqn{p}{p}.
#'  
#' Covariates effect parameter \eqn{\theta}{T} 
#' via the log link function, i.e., \eqn{\theta = exp(x'b)}{T = exp(x'b)},
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
#' # theta profile likelihood 
#' # theta= 10, p = 100 / 120 = 0.833
#' whi <- 50
#' x <- c( runif(100, min=0, max=10), runif(20, min=10, max=whi))
#' Xmat <- matrix(1, nrow=length(x), ncol = 1)
#' T <- seq(0.01, whi, length=300)
#' nLLVec <- NULL
#' for (tt in T){ 
#'   ll <- oneStep.like(a = c(log(tt), 0.833)
#'                    , dist = x
#'                    , covars = Xmat
#'                    ,  w.hi = whi)
#'   nLLVec <- c(nLLVec, -sum(log(ll$L.unscaled)))
#' }
#' plot(T, nLLVec, type="l")
#'
#' 
#' # Estimated oneStep distance function ----
#' whi <- units::set_units(250, "m")
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
#' fit <- dfuncEstim(distDf
#'                  , formula = dist ~ 1
#'                  , likelihood = "oneStep"
#'                  , w.hi = units::set_units(250, "m")
#'                  )
#' theta <- exp(fit$par[1]) # take next order statistics >theta
#' p <- fit$par[2]
#' tmp <- hist(x, n = 30, plot=F)
#' scalar <- p / theta
#' tmp$density <- tmp$density / scalar
#' y2 <- ((1-p)*theta) / ((whi-theta)*p) # or (1-p)/(whi-theta)/scalar
#' barplot(tmp$density
#'       , width = diff(tmp$breaks[1:2])
#'       , space = 0
#' )
#' axis(1)
#' lines(x=c(0,theta), y = c(1,1), col="red", lwd = 2)
#' lines(x=c(theta,whi), y = rep(y2,2), col="red", lwd=2)
#' lines(x=c(theta,theta), y = c(1,y2), col="red", lwd=2)
#' 
#' # ============= two thetas =============================
#' # theta 1 = 10, theta 2 = 20
#' whi <- 50
#' n <- c(200, 200)
#' p <- 0.8
#' theta <- c(10,20)
#' rp <- rbinom(sum(n), 1, p)
#' i1 <- seq(1, n[1])
#' i2 <- seq(n[1]+1, n[1]+n[2])
#' x <- c( 
#'   rp[i1]*runif(n[1], min=0, max=theta[1]) + (1-rp[i1])*runif(n[1], min=theta[1], max=whi)
#' , rp[i2]*runif(n[2], min=0, max=theta[2]) + (1-rp[i2])*runif(n[2], min=theta[2], max=whi)
#'       )
#' Xmat <- matrix(1, nrow=length(x), ncol = 1)
#' Xmat <- cbind( Xmat, matrix(c(rep(0,n[1]), rep(1,n[2])), nrow=length(x), 1))
#' 
#' library(dfoptim)
#' strt <- c(log(5), log(5), 0.5) # get ml start value from 'step1Estimates.R'
#' lowLim <- c(log(1e-3), log(1e-3), 1e-3)
#' higLim <- c(log(whi-1e-3), log(whi-1e-3), 1 - 1e-3)
#' fit <- dfoptim::hjkb(
#'     par = strt
#'   , fn = nLL
#'   , lower = lowLim
#'   , upper = higLim
#'   , dist = x
#'   , covars = Xmat
#'   , w.hi = whi
#' )
#' 
#' thetaHat <- matrix(c(1,1,0,1), 2,2) %*% matrix(fit$par[1:2],2,1)
#' thetaHat <- exp(thetaHat)
#' p <- fit$par[3]
#' scalar <- p / thetaHat
#' y2 <- ((1-p)*thetaHat) / ((whi-thetaHat)*p) # or (1-p)/(whi-theta)/scalar
#' 
#' par(mfrow=c(1,2))
#' tmp <- hist(x[i1], n = 30, plot=F)
#' tmp$density <- tmp$density / scalar[1]
#' barplot(tmp$density
#'       , width = diff(tmp$breaks[1:2])
#'       , space = 0
#' )
#' axis(1)
#' lines(x=c(0,thetaHat[1]), y = c(1,1), col="red", lwd = 2)
#' lines(x=c(thetaHat[1],whi), y = rep(y2[1],2), col="red", lwd=2)
#' lines(x=c(thetaHat[1],thetaHat[1]), y = c(1,y2[1]), col="red", lwd=2)
#' 
#' tmp <- hist(x[i2], n = 30, plot=F)
#' tmp$density <- tmp$density / scalar[2]
#' barplot(tmp$density
#'       , width = diff(tmp$breaks[1:2])
#'       , space = 0
#' )
#' axis(1)
#' lines(x=c(0,thetaHat[2]), y = c(1,1), col="blue", lwd = 2)
#' lines(x=c(thetaHat[2],whi), y = rep(y2[2],2), col="blue", lwd=2)
#' lines(x=c(thetaHat[2],thetaHat[2]), y = c(1,y2[2]), col="blue", lwd=2)
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
  key <- (p / theta) * (0 <= dist & dist <= theta) + 
         ((1-p) / (w.hi - theta)) * (theta < dist & dist <= w.hi)

  return( list(L.unscaled = key, 
               params = theta))

}