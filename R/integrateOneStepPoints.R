#' @title Integrate Point-survey One-step function
#' 
#' @description
#' Compute integral of the one-step distance function
#' for point-surveys. 
#' 
#' @param object Either an Rdistance fitted distance function,
#' (an object that inherits from class "dfunc"; usually produced 
#' by a call to \code{\link{dfuncEstim}}), or a matrix of canonical 
#' distance function parameters (e.g., matrix(fit$par,1)). 
#' If a matrix, each row corresponds to a 
#' distance function and each column is a parameter. Argument 
#' \code{newdata} is ignored if \code{object} is a matrix.
#' 
#' @param w.hi Maximum sighting distance. Ignored if \code{object} 
#' is a fitted Rdistance distance function.
#' 
#' @param Units Physical units to apply to the first column of 
#' \code{object} when it is a matrix.  Ignored if \code{object}
#' is a fitted Rdistance distance function.
#' 
#' @inheritParams effectiveDistance 
#' 
#' @details 
#' Returned integral is exact.
#' 
#' @return A vector of areas under distance functions. 
#' If \code{object} is a distance function and 
#' \code{newdata} is specified, return length is 
#' \code{nrow(newdata)}.  If \code{object} is a distance function and 
#' \code{newdata} is NULL, 
#' return length is \code{length(distances(object))}. If 
#' \code{object} is a matrix of parameters, return length is 
#' \code{nrow(object)}. 
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
#' Theta <- units::set_units(Theta, "m")
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
                            , w.hi = NULL
                            , Units = NULL
                              ){

  # need this if b/c sometimes this is called from nLL (object is just a 
  # matrix of parameters) and other times it is called from EDR (object is 
  # fitted object)
  if( inherits(object, "dfunc") ){
    w.hi <- object$w.hi # override input if it's given
    Units <- object$outputUnits # override if given
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
  } 
  
  Theta <- units::set_units(object[,1], Units, mode = "standard")
  p <- object[,2]
  fatT <- (((1-p) * Theta) / ((w.hi - Theta) * p)) # height of f from Theta to w.hi

  # Triangle between 0 and Theta
  part1 <- Theta * Theta / 2
  # Trapazoid between Theta and w.hi
  gAtT <- fatT * Theta
  gAtw <- fatT * w.hi
  part2 <- (gAtw + gAtT) * (w.hi - Theta) / 2 
  
  outArea <- part1 + part2
  
  outArea # units should be object$outputUnits^2
  
}