#' @title Integrate Line-transect One-step function 
#' 
#' @description
#' Compute integral of the one-step distance function for line 
#' transects. 
#' 
#' @inheritParams integrateOneStepPoints 
#' @inheritParams effectiveDistance 
#' 
#' @details 
#' Returned integral is exact.
#' 
#' @inherit integrateOneStepPoints return
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexp}}; 
#' \code{\link{integrateHalfnorm}} 
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
  
  Theta <- units::set_units(object[,1], Units, mode = "standard")
  p <- object[,2]

  outArea <- Theta / p
  

  outArea 
  
}