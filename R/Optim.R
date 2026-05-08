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
  optimMeth <- ml$optimizer
  optimSubMeth <- sub("^.+_", "", optimMeth)
  
  if( verboseLevel >= 1 ){
    cat(colorize(paste(
        optimMeth
      , "maximization"
      , "----\n"), col = "red"))
  }

  if( optimSubMeth %in% c("L-BFGS-B") ){
    # can use limits, else no
    fit <- stats::optim(
        par = strt.lims$start
      , fn = nLL
      , lower = dropUnits(strt.lims$low) # safe
      , upper = dropUnits(strt.lims$high)
      , hessian = TRUE
      , control = contRl
      , method = optimSubMeth
      , ml = ml
      , verbosity = verboseLevel
    )
  } else {
    fit <- stats::optim(
        par = strt.lims$start
      , fn = nLL
      , hessian = TRUE
      , control = contRl
      , method = optimSubMeth
      , ml = ml
      , verbosity = verboseLevel
    )
    
  }
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