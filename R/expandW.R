#' @title Domain of expansion factors
#' 
#' @description
#' The domain to which expansion factors are applied varies by likelihood. 
#' When the domain varies, it depends on parameter values. For example, 
#' \code{oneStep} expansion factors are applied between 
#' 0 and the likelihood's first parameter (\eqn{\theta}{Theta}), which 
#' varies by covariates.  This function computes the likelihood-specific 
#' expansion domains
#' 
#' @inheritParams startLimits
#' 
#' @param params A matrix of likelihood parameters.  Size is (number 
#' of distances) X [(number of cases) + (number of non-covariate parameter)].
#' 
#' @param k The number of cases.
#' 
#' @return A plain vector of length k containing expansion domains, with units.  
#' 
#' @export
expandW <- function(ml
                  , params
                  , k
                  ){
  # oneStep and triangle and huber have expansion W that varies
  # (expansion W is NOT the real w = strip width)
    
  if(!(ml$likelihood %in% differentiableLikelihoods())){
    # Expansion domain depends on parameters.
    # e.g., Apply expansion between 0 and theta for triangle
    W <- params[1, 1:k] # drop second parameter which is tagged onto end
    W <- setUnits(exp(W), ml$outputUnits) # W always constant w/in columns
    W <- W - ml$w.lo
    if( ml$likelihood %in% c("huber") ){
      # e.g., Apply expansion between 0 and theta for oneStep, triangle
      theta2 <- setUnits(params[1, k+1], ml$outputUnits)
      W <- W + theta2
      W[W > (ml$w.hi - ml$w.lo)] <- ml$w.hi - ml$w.lo
    }

  } else { 
    # Most likelihoods: expansions constant across params
    W <- ml$w.hi - ml$w.lo
  }

  W
}