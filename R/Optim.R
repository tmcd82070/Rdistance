#' @title 'optim' optimizer
#' 
#' @description
#' Call R native function 'optim' to perform optimization. 
#' 
#' @inheritParams mlEstimates
#' 
#' @inherit Nlminb return
#' 
# Do Not export

Optim <- function(ml, strt.lims){
  
  contRl <- list(trace = getOption("Rdistance_trace")
                 , maxit = getOption("Rdistance_maxIters")
                 , factr = getOption("Rdistance_likeTol")
                 , pgtol = getOption("Rdistance_likeTol")
  )
  
  verboseLevel <- getOption("Rdistance_verbosity")
  if( verboseLevel >= 1 ){
    cat(colorize("OPTIM gradient method L-BFGS-B maximization ----\n", col = "red"))
  }
  
  fit <- stats::optim(
      par = strt.lims$start
    , fn = nLL
    , lower = units::set_units(strt.lims$low, NULL) # safe
    , upper = units::set_units(strt.lims$high, NULL)
    , hessian = TRUE
    , control = contRl
    , method = c("L-BFGS-B")
    , ml = ml
    , verbosity = verboseLevel
  )
  
  names(fit$par) <- strt.lims$names
  if( ml$asymptoticSE ){
    fit$varcovar <- Rdistance::varcovarEstim(fit, ml)
  } else {
    fit$varcovar <- NULL
  }
  fit$hessian <- NULL  # erase hessian from fit object

  # final few things ----
  fit$limits <- strt.lims[c("low", "high")]

  # Flip over objective: object$logLike is true logLike, -LL was minimized
  names(fit)[names(fit) == "value"] <- "loglik"
  fit$loglik <- -fit$loglik  
  
  fit$evaluations <- fit$counts[1]
  fit$counts <- NULL
  
  names(fit)[names(fit) == "feval"] <- "evaluations"
  names(fit)[names(fit) == "niter"] <- "iterations"
  
  
  fit
} 