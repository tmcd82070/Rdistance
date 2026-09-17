#' @title OSCARS optimizer
#' 
#' @description
#' Uses the OSCARS optimization algorithm implemented 
#' in `OSCARS::oscars()` to perform maximum likelihood estimation of 
#' a distance function. 
#' 
#' @inheritParams mlEstimates
#'
#' @inherit Nlminb return
#' 
# Do Not export

Oscars <- function(ml, strt.lims){
  
    
  contRl <- list(infol = getOption("Rdistance_trace") 
                 , DoMax = FALSE
                 , nfmax = getOption("Rdistance_oscarEvals")
                 , fTol = getOption("Rdistance_oscarTol")
                 , xTol = getOption("Rdistance_coefTol") 
  )
  
  verboseLevel <- getOption("Rdistance_verbosity")
  if( verboseLevel >= 1 ){
    cat(colorize("OSCARS non-gradient maximization ----\n", col = "red"))
  }
  
  fit <- OSCARS::oscars(
      fname = nLL
    , n = length(strt.lims$start)
    , lwr = strt.lims$low
    , upr = strt.lims$high
    , ml = ml
    , start = strt.lims$start
    , controls = contRl
  )
  
  names(fit$par) <- strt.lims$names
  if( ml$asymptoticSE ){
    fit$varcovar <- Rdistance::varcovarEstim(fit, ml)
  } else {
    fit$varcovar <- NULL
  }

  # final few things ----
  fit$limits <- strt.lims[c("low", "high")]
  fit$limits$startConvergence = strt.lims$convergence
  fit$limits$startLogLik = strt.lims$loglik
  
  # Flip over objective: object$logLike is true logLike, -LL was minimized
  names(fit)[names(fit) == "value"] <- "loglik"
  fit$loglik <- -fit$loglik  
  
  names(fit)[names(fit) == "feval"] <- "evaluations"
  names(fit)[names(fit) == "niter"] <- NA_integer_
  
  if( fit$convergence == 0 & fit$evaluations < contRl$nfmax ){
    fit$message <- "converged (Oscars)"
  } else if( strt.lims$convergence == 0 && strt.lims$loglik >= fit$loglik ) {
    fit$message <- "converged (NR)"
    fit$convergence <- 0
  } else if( strt.lims$convergence == 0 && strt.lims$loglik < fit$loglik ) {
    fit$message <- "converged (Oscars improved)"
    fit$convergence <- 0
  } else {
    fit$message <- "did not converge"
  }
  
  fit
} 