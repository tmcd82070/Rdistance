#' @title Integrate Point survey One-step function
#' 
#' @description
#' Compute integral of the one-step distance function
#' for point-surveys. 
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
integrateOneStepPoints <- function(object
                            , newdata = NULL
                              ){

  y <- stats::predict(object = object
                      , newdata = newdata
                      , type = "parameters"
  )
  
  Theta <- y[,1]

  w.hi <- units::set_units(object$w.hi, NULL)
  
  x <- c(cbind(0
             , Theta
             , Theta + 100*getOption("Rdistance_fuzz")
             ,  w.hi))  # matrix in vector form
  x <- units::set_units(x, object$outputUnits, mode = "standard")
  
  y <- stats::predict(object = object
                      , newdata = newdata
                      , type = "dfuncs"
                      , distances = x
  )
  
  # rows are parameters, and pages are parameters
  x <- array( c(x), dim = c(nrow(newdata), 4, nrow(newdata)))
  y <- array( c(y), dim = c(nrow(newdata), 4, nrow(newdata)))
  
  xy <- x*y
  
  # Triangle between 0 and Theta
  part1 <- 0.5 * xy[1,2,] # should be length(nrow(newdata))
  
  # Trapazoid between Theta and w.hi
  gAtT <- xy[1,3,]
  gAtw <- xy[1,4,]
  part2a <- (gAtw - gAtT) * (w.hi - Theta) / 2 # triangle
  part2b <- gAtT * (w.hi - Theta) # base
  
  outArea <- part1 + part2a + part2b
  
  outArea <- sqrt( 2 * outArea)
  
  outArea <- units::set_units(outArea
                              , object$outputUnits
                              , mode = "standard")
  
  outArea 
  
}