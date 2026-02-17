#' @title Mixture of triangle and uniform likelihood
#' 
#' @description
#' Compute likelihood function for a mixture of a triangle and 
#' uniform distributions. 
#' 
#' @inheritParams halfnorm.like
#' 
#' @inherit halfnorm.like return seealso
#' 
#' @details Rdistance's \code{triangle} likelihood is a mixture of a 
#' triangle and uniform distribution. The 'triangle' density function
#' is  
#' \deqn{f(d|\theta) = (1 - \frac{1-p}{\theta}d) I(0 \leq d \leq \theta) + 
#'        p I(\theta \le d \leq w),}{
#'        f(d|T) = (1 - (1-p)*d/T)I(0<=d<=T) + p*I(T<d<=w),}
#' where 
#' \eqn{I(x)} is the indicator function for event \eqn{x}, 
#' and \eqn{w} is the nominal strip width 
#' (i.e., \code{w.hi - w.lo} in Rdistance). 
#' The unknown parameters to be estimated 
#' are \eqn{\theta}{T} and \eqn{p} 
#' (\eqn{w} is fixed - given by the user). 
#'  
#' Covariates influence values of \eqn{\theta}{T} 
#' via a log link function, i.e., \eqn{\theta = e^{x'b}}{T = exp(x'b)},
#' where \eqn{x} is the vector of covariate values 
#' associated with distance \eqn{d}, and \eqn{b}
#' is the vector of estimated coefficients. 
#' 
#' @examples
#' 
#' w <- 250
#' T <- 160
#' p <- 0.4
#' d <- seq(0,w,length = w+1)
#' y <- (1 - ((1-p)/T)*d)*(d <= T) + p*(d > T)
#' plot(d, y, type="l", ylim = c(0,1), xlab = "Distance", ylab = "Probability")
#' points(T, p, pch=16, col = "red")
#' lines(c(-10,T), c(p,p), lty = 2, col = "grey")
#' axis(2, at=p, label = "p", line = 2, srt = 0, tick = F)
#' lines(c(T,T), c(-1,p), lty = 2, col = "grey")
#' axis(1, at=T, label = "T", line = 2, tick = F)
#' 
#' T <- 25
#' p <- 0.2
#' y <- (1 - ((1-p)/T)*d)*(d <= T) + p*(d > T)
#' lines(d, y)
#' points(T, p, pch=16, col = "red")
#' lines(c(-10,T), c(p,p), lty = 2, col = "grey")
#' axis(2, at=p, label = "p", line = 2, srt = 0, tick = F)
#' lines(c(T,T), c(-1,p), lty = 2, col = "grey")
#' axis(1, at=T, label = "T", line = 2, tick = F)
#' 
#' # same as above
#' y <- triangle.like(a = c(log(T), p)
#'                  , dist = d
#'                  , covars = matrix(1, length(d))
#'                  , w.hi = 250)
#' lines(d, y$L.unscaled, col = "green")
#' 
#' @export
triangle.like <- function(a
                , dist
                , covars 
                , w.hi = NULL) {
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  q <- Rdistance:::nCovars(covars) 
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
    p <- a[1, q+1, drop = TRUE]     # 1 X 1
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    p <- a[q+1]     # 1 X 1
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  theta <- exp(s)  # link function here
  
  dist <- dropUnits(dist)

  if(is.null(w.hi)){
    w.hi <- max(dist)  # no units b/c removed above
  } else {
    w.hi <- dropUnits(w.hi) # already checked units
  }

  # 'p' is height for d > theta
  m <- (1 - p) / theta     # slope btwn 0 and theta
  key <- (1 - m*dist) * (0 <= dist & dist <= theta) + 
    p * (theta < dist & dist <= w.hi)
  
  return( list(L.unscaled = key, 
               params = cbind(s, p)) )

}
