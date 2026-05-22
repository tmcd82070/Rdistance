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
  
  here!!!
    
  contRl <- list(info = getOption("Rdistance_trace") > 0
                 , maximize = FALSE
                 , target = Inf
                 , maxfeval = getOption("Rdistance_evalMax")
                 , tol = getOption("Rdistance_likeTol")
  )
  
  verboseLevel <- getOption("Rdistance_verbosity")
  if( verboseLevel >= 1 ){
    cat(colorize("HOOKEJEEVES non-gradient maximization ----\n", col = "red"))
  }
  
  fit <- OSCARS::oscars(
      par = strt.lims$start
    , fn = nLL
    , lower = strt.lims$low
    , upper = strt.lims$high
    , control = contRl
    , verbosity = verboseLevel
    , ml = ml
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
  names(fit)[names(fit) == "niter"] <- "iterations"
  
  if( fit$convergence == 0 ){
    fit$message <- "converged"
  } else {
    fit$message <- "did not converge"
  }
  
  fit
} 