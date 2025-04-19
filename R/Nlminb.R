#' @title 'nlminb' optimizer
#' 
#' @description
#' Call R native function 'nlminb' to perform optimization. 
#' 
#' @inheritParams mlEstimates
#' 
#' @return 
#' A list with following named components:
#' \itemize{
#'   \item par = parameters
#'   \item loglik = objective function value at minimum
#'   \item convergence = 0 for yes, other for no
#'   \item iterations = number of iterations
#'   \item evaluations = function evaluations
#'   \item message = a convergence message
#'   \item varcovar = a variance covariance matrix of parameters
#'   \item limits = low and high limits
#' }
#' 
# Do Not export

Nlminb <- function(ml, strt.lims){
  
  contRl <- list(trace = getOption("Rdistance_trace")
                 , eval.max = getOption("Rdistance_evalMax")
                 , iter.max = getOption("Rdistance_maxIters")
                 , rel.tol = getOption("Rdistance_likeTol")
                 , x.tol = getOption("Rdistance_coefTol")
  )
  
  fit <- stats::nlminb(
    start = strt.lims$start
    , objective = nLL
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
  names(fit)[names(fit) == "objective"] <- "loglik"
  fit$loglik <- -fit$loglik  

  
  fit
} 