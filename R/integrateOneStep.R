#' @title Integrate One-step function 
#' 
#' @description
#' Compute integral of the one-step distance function. 
#' 
#' @inheritParams effectiveDistance
#' 
#' @details 
#' Returned integral is exact.
#' 
#' @return A vector of areas under distance functions. 
#' If \code{newdata} is specified, return length is 
#' \code{nrow(newdata)}.  If \code{newdata} is NULL, 
#' return length is \code{length(distances(object))}. 
#' 
#' @seealso [integrateNumeric()]; [integrateNegexp()]; 
#' [integrateOneStep()] 
#' 
#' @examples
#' 
#' # Check:
#' w.hi <- 125
#' w.lo <- 0
#' s1 <- 40
#' s2 <- exp(log(s1) + log(0.5))
#' obs1Scaler <- (pnorm(w.hi, mean=w.lo, sd = s1) - 0.5) * sqrt(2*pi)*s1
#' obs2Scaler <- (pnorm(w.hi, mean=w.lo, sd = s2) - 0.5) * sqrt(2*pi)*s2
#' c(obs1Scaler, obs2Scaler)
#' 
#' @export
#' 
integrateOneStep <- function(object
                            , newdata = NULL
                              ){

  y <- stats::predict(object = object
                      , newdata = newdata
                      , type = "parameters"
  )
  
  Theta <- y[,1]
  p <- y[,2]
  
  w.lo <- units::set_units(object$w.lo, NULL)

  outArea <- Theta / p
  
  outArea <- units::set_units(outArea
                              , object$outputUnits
                              , mode = "standard")
  
  outArea 
  
}