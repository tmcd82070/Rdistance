#' @title Integrate Gamma line surveys
#' 
#' @description
#' Compute integral of the Gamma distance function for 
#' line-transect surveys.
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' #' Returned integrals are
#' \deqn{\int_0^{w} \left(\frac{x}{m}\right)^{\alpha -1}  e^{-(x - m)/\sigma_i}dx,}{
#' Integral( (x/m)^(a-1) exp(-(x-m)/s_i) ),}
#' where \eqn{w = w.hi - w.lo}, \eqn{\sigma_i}{s_i} is the i-th estimated scale 
#' parameter for the Gamma distance function, and \eqn{m} is the mode of Gamma
#' (i.e., \eqn{(\alpha - 1)\sigma_i}{(a-1)s_i}. 
#' Rdistance computes the integral using R's base function 
#' \code{pgamma()}, which for all intents and purposes is exact. 
#' See also \code{\link{Gamma.like}}.
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}}; 
#' \code{\link{integrateOneStepLines}} 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- rep(1,4) %m%. # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(4.0, -0.5, 1.5) # {'Intercept', b_1, shape}
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , par = beta 
#'   , likelihood = "Gamma"
#'   , w.lo = w.lo %#% "m"
#'   , w.hi = w.hi %#% "m"
#' )
#' class(ml) <- "dfunc"
#' integrateGammaLines(ml)
#' 
#' # Check: Integral of Gamma density from 0 to w.hi-w.lo
#' b <- exp(c(beta[1], beta[1] + beta[2]))
#' B <- Rdistance::GammaReparam(shp = beta[3], scl = b)
#' intgral <- pgamma(q = w.hi - w.lo, shape = B$shp, scale = B$scl) 
#' intgral
#' 
#' 
#' @export
#' 
integrateGammaLines <- function(object
                              , newdata = NULL
                              , w.lo = NULL
                              , w.hi = NULL
                              , Units = NULL
                                ){

  if( inherits(object, "dfunc") ){
    Units <- object$outputUnits
    w.lo <- object$w.lo
    w.hi <- object$w.hi
    object <- stats::predict(object = object
                             , newdata = newdata
                             , type = "parameters"
    )
  } 
  
  w <- dropUnits(w.hi) - dropUnits(w.lo)
  
  dgamPars <- GammaReparam(scl = object[,1]
                           , shp = object[,2])
  
  # Note: we use pgamma and dgamma to avoid taking gamma(<big number>)
  # which is Inf. If gamma(dgamPars$shp) is < Inf, the integral is:
  # pgamma(w, shp, scale = scl) * 
  #       scl^shp * gamma(shp) / 
  #       ((shp - 1)*scl)^(shp - 1) * exp(1 - shp)
  
  outArea <-  stats::pgamma(q = w
                          , shape = dgamPars$shp
                          , scale = dgamPars$scl
                            ) 
              
  
  # Raw gamma is scaled to max = 1
  m <- (dgamPars$shp - 1)*dgamPars$scl # mode
  f.at.m <- stats::dgamma( m, shape=dgamPars$shp, scale=dgamPars$scl )
  outArea <- outArea / f.at.m
  
  outArea <- setUnits(outArea, Units)
  
  outArea 
  
}