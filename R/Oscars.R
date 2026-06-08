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
                 , nfmax = getOption("Rdistance_evalMax")
                 , fTol = getOption("Rdistance_likeTol")
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
  
  # Flip over objective: object$logLike is true logLike, -LL was minimized
  names(fit)[names(fit) == "value"] <- "loglik"
  fit$loglik <- -fit$loglik  
  
  names(fit)[names(fit) == "feval"] <- "evaluations"
  names(fit)[names(fit) == "niter"] <- NA_integer_
  
  if( fit$convergence == 0 ){
    fit$message <- "converged"
  } else {
    fit$message <- "did not converge"
  }
  
  fit
} 