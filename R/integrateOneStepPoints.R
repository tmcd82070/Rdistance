#' @title Integrate Point-survey One-step function
#' 
#' @description
#' Compute integral of the one-step distance function
#' for point-surveys. 
#' 
#' @param object Either an Rdistance fitted distance function
#' (an object that inherits from class "dfunc"; usually produced 
#' by a call to \code{\link{dfuncEstim}}), or a matrix of canonical 
#' distance function parameters (e.g., \code{matrix(exp(fit$par),1)}). 
#' If a matrix, each row corresponds to a 
#' distance function and each column is a parameter. The first column is 
#' the parameter related to sighting covariates and must be transformed 
#' to the "real" space (i.e., inverse link, which is \eqn{exp()}, must 
#' be applied outside this routine).  If \code{object} is a matrix, 
#' it should not have measurement units because
#' only derived quantities (e.g., ESW) have units; Rdistance function 
#' parameters themselves never have units.
#' 
#' @param newdata A data frame containing new values for 
#' the distance function covariates. If NULL and 
#' \code{object} is a fitted distance function, the  
#' observed covariates stored in
#' \code{object} are used (behavior similar to \code{\link{predict.lm}}).
#' Argument \code{newdata} is ignored if \code{object} is a matrix.
#' 
#' @param w.lo Minimum sighting distance or left-truncation value
#' if \code{object} is a matrix.
#' Ignored if \code{object} 
#' is a fitted distance function. 
#' Must have physical measurement units. 
#' 
#' @param w.hi Maximum sighting distance or right-truncation value
#' if \code{object} is a matrix.
#' Ignored if \code{object} 
#' is a fitted distance function.
#' Must have physical measurement units. 
#' 
#' @param Units Physical units of sighting distances if 
#' \code{object} is a matrix. Sighting distance units can differ from units 
#' of \code{w.lo} or \code{w.hi}.   Ignored if \code{object}
#' is a fitted distance function.
#' 
#' @section Note:
#' Users will not normally call this function. It is called 
#' internally by \code{\link{nLL}} and \code{\link{effectiveDistance}}. 
#' 
#' @details 
#' Returned integrals are
#' \deqn{\int_0^{w} x(\frac{p}{\theta_i}I(0\leq x \leq \theta_i) + \frac{1-p}{w - \theta_i}I(\theta_i < x \leq w)) dx = \frac{\theta_i}{2p}((1-p)w + \theta_i),}{
#' Integral(x((p/Theta)I(0<=x<=Theta) + ((1-p)/(w-Theta))I(Theta<x<=w))) = Theta ((1-p)w + Theta) / (2p),} 
#' where \eqn{w = w.hi - w.lo}, \eqn{\theta_i}{Theta} is the estimated one-step
#' distance function
#' threshold for the i-th observed distance, and \eqn{p}{p} is the estimated
#' one-step proportion.
#' 
#' @return A vector of areas under the distance functions represented in 
#' \code{object}. 
#' If \code{object} is a distance function and 
#' \code{newdata} is specified, the returned vector's length is 
#' \code{nrow(newdata)}.  If \code{object} is a distance function and 
#' \code{newdata} is NULL, 
#' returned vector's length is \code{length(distances(object))}. If 
#' \code{object} is a matrix, return's length is 
#' \code{nrow(object)}. 
#' 
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateOneStepNumeric}}; 
#' \code{\link{integrateOneStepLines}} 
#' 
#' @examples
#' 
#' fit <- dfuncEstim(thrasherDf, dist~1, likelihood = "oneStep")
#' integrateOneStepPoints(fit, newdata = data.frame(`(Intercept)`=1))
#' EDR(fit, newdata = data.frame(`(Intercept)`=1))
#' 
#' # Check: 
#' Theta <- exp(fit$par[1])
#' Theta <- setUnits(Theta, "m")
#' p <- fit$par[2]
#' w.hi <- fit$w.hi
#' w.lo <- fit$w.lo
#' g.at0 <- w.lo
#' g.atT <- Theta
#' g.atTPlusFuzz <- (((1-p) * Theta) / ((w.hi - Theta) * p))*Theta
#' g.atWhi <- (((1-p) * Theta) / ((w.hi - Theta) * p))*w.hi
#' area.0.to.T <- (Theta - w.lo) * (g.atT - g.at0) / 2 # triangle; Theta^2/2
#' area.T.to.w <- (w.hi - Theta) * (g.atTPlusFuzz + g.atWhi) / 2 # trapazoid
#' area <- area.0.to.T + area.T.to.w
#' edr <- sqrt( 2*area )
#' 
#' @export
#' 
integrateOneStepPoints <- function(object
                            , newdata = NULL
                            , w.lo = NULL
                            , w.hi = NULL
                            , Units = NULL
                              ){

  # need this if b/c sometimes this is called from nLL (object is just a 
  # matrix of parameters) and other times it is called from EDR (object is 
  # fitted object)
  if( inherits(object, "dfunc") ){
    w.hi <- object$w.hi # override input if it's given
    w.lo <- object$w.lo
    Units <- object$outputUnits # override if given
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
  } 
  
  Theta <- setUnits(object[,1], Units)
  p <- object[,2]
  w <- w.hi - w.lo # must have units, or calculation fails below.

  # Slower, but clearer:
  # fatT <- (((1-p) * Theta) / ((w.hi - Theta) * p)) # height of f from Theta to w.hi
  #   Triangle between 0 and Theta
  # part1 <- Theta * Theta / 2
  #   Trapazoid between Theta and w.hi
  # gAtT <- fatT * Theta
  # gAtw <- fatT * w.hi
  # part2 <- (gAtw + gAtT) * (w.hi - Theta) / 2 
  # outArea <- part1 + part2
  
  outArea <- 0.5 * Theta * ((1 - p) * w + Theta) / p
  
  outArea # units should be object$outputUnits^2
  
}
