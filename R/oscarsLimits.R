#' @title Modify start and limits for OSCARS optimization
#' 
#' @description Improve (tighten) the starting values and 
#' limits for the OSCARS algorithm. OSCARS needs tighter bounds
#' because of it's global nature. 
#' 
#' @param ml A "model list" containing the problem parameters. 
#' 
#' @param origLims A list of $start, $low, and $high components 
#' that are vectors of the same length.  $start are the starting 
#' values, $low are the lower bounds, and $high are the upper bounds
#' on the coefficients in the model. 
#' 
#' @return A list just like `curLims` with improved start and 
#' limits.  In addition, the return contains a couple quantities used
#' later to assess convergence (i.e., $startConvergence and 
#' $startEvaluations)
#'
#' @details
#' Despite several likelihoods being discontinuous, gradient based
#' methods do amazingly well.  We improve (tighten) the 
#' uninformed start limites by estimating the model
#' using Newton-Raphson and constructing limits from those estimates. 
#'  
# Un-exported

oscarsLimits <- function(origLims, ml){

  ml$optimizer <- "nlminb"
  elp <- system.time( 
    fitNR <- mlEstimates(ml, origLims)
  )
  secPerEval <- elp["elapsed"] / sum(fitNR$evaluations)
  estEvalTime <- getOption("Rdistance_oscarEvals") * secPerEval / 60.0
  estEvalTime <- setUnits(estEvalTime, "minutes")
  if (getOption("Rdistance_verbosity") >= 0) {
    cat(paste0("Estimated OSCARS run time <= ", colorize(format(round(estEvalTime, 3))), ".\n"))
  }
  
  startNR <- fitNR$par
  
  hessian <- Rdistance::secondDeriv(
      x = startNR
    , FUN = nLL
    , eps = getOption("Rdistance_hessEps")
    , ml = ml
    , verbosity = 0
  )
  seNR <- tryCatch(solve(hessian), error = function(e){NaN})
  if( is.matrix(seNR) ){
    seNR <- diag(seNR)
    seNR <- ifelse(is.na(seNR) | (seNR < 0), abs(startNR), seNR) # For missing SE's
    seNR <- sqrt(seNR)
  } else {
    seNR <- sqrt(abs(startNR))
  }
  
  strtLims <- origLims
  strtLims$start <- startNR
  strtLims$low  <- startNR - 6 * seNR
  strtLims$high <- startNR + 6 * seNR
  
  # Add a couple elements to strtLims not present for other 
  # likelihoods.  Used later in Oscars()
  strtLims$loglik <- fitNR$loglik
  strtLims$convergence <- fitNR$convergence
  strtLims$estEvalTime <- estEvalTime
  
  # hard limits at the incoming limits
  strtLims$low  <- pmax(strtLims$low, origLims$low)
  strtLims$high <- pmin(strtLims$high, origLims$high)
  
  strtLims
  
}