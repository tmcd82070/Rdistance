#' @title 'nlminb' optimizer
#' 
#' @description
#' Call R native function 'nlminb' to perform optimization. 
#' 
#' @inheritParams mlEstimates
#'
#' @inherit Nlminb return
#' 
# Do Not export

HookeJeeves <- function(ml, strt.lims){
  
  contRl <- list(info = getOption("Rdistance_trace") > 0
                 , maximize = FALSE
                 , target = Inf
                 , maxfeval = getOption("Rdistance_evalMax")
                 , tol = getOption("Rdistance_likeTol")
  )
  
  fit <- dfoptim::hjkb(
      par = strt.lims$start
    , fn = nLL
    , lower = strt.lims$low
    , upper = strt.lims$high
    , control = contRl
    , ml = ml
  )
  
  names(fit$par) <- strt.lims$names
  fit$varcovar <- Rdistance::varcovarEstim(fit, ml)

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