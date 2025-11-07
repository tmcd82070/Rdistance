#' @title Integrate Line-transect One-step function 
#' 
#' @description
#' Compute exact integral of the one-step distance function for line 
#' transects. 
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' Returned integrals are
#' \deqn{\int_0^{w} (\frac{p}{\theta_i}I(0\leq x \leq \theta_i) + 
#'    \frac{1-p}{w - \theta_i}I(\theta_i < x \leq w)) dx = \frac{\theta_i}{p},}{
#' Integral((p/Theta)I(0<=x<=Theta) + ((1-p)/(w-Theta))I(Theta<x<=w)) = Theta / p,} 
#' where \eqn{w = w.hi - w.lo}, \eqn{\theta_i}{Theta} is the estimated one-step
#' distance function
#' threshold for the i-th observed distance, and \eqn{p}{p} is the estimated 
#' one-step proportion. 
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateHalfnormLines}} 
#' 
#' @examples
#' 
#' # A oneStep distance function on simulated data
#' whi <- 250
#' T <- 100  # true threshold
#' p <- 0.85 # true proportion <T
#' n <- 200  # num simulated points
#' x <- c( runif(n*p, min=0, max=T), runif(n*(1-p), min=T, max=whi))
#' x <- setUnits(x, "m")
#' tranID <- sample(rep(1:10, each=n/10), replace=FALSE)
#' detectDf <- data.frame(transect = tranID, dist = x)
#' siteDf <- data.frame(transect = 1:10
#'                , length = rep(setUnits(10,"m"), 10))
#' distDf <- RdistDf(siteDf, detectDf)
#' 
#' # Estimation
#' fit <- dfuncEstim(distDf
#'                  , formula = dist ~ 1
#'                  , likelihood = "oneStep"
#'                  , w.hi = setUnits(whi, "m")
#'                  )
#' table(integrateOneStepLines(fit))
#' table(ESW(fit))
#' 
#' # Check:
#' T.hat <- exp(fit$par[1])
#' p.hat <- fit$par[2]
#' gAtT <- ((1-p.hat) * T.hat) / (p.hat * (whi - T.hat))
#' 
#' plot(fit)
#' abline(h = gAtT, col="blue")
#' 
#' areaLE.T <- (1.0) * T.hat
#' areaGT.T <- gAtT * (whi - T.hat)
#' areaLE.T + areaGT.T  # ESW
#' 
#' # Equivalent
#' T.hat / p.hat
#' 
#' @export
#' 
integrateOneStepLines <- function(object
                            , newdata = NULL
                            , Units = NULL
                              ){

  if( inherits(object, "dfunc") ){
    Units <- object$outputUnits
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
  } 
  
  Theta <- setUnits(object[,1], Units)
  p <- object[,2]

  outArea <- Theta / p
  

  outArea 
  
}
