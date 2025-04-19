#' @title Estimate variance-covariance
#' 
#' @description
#' Estimate the variance-covariance matrix of parameters 
#' in the distance function. If the likelihood is 
#' differentiable, the variance-covariance matrix is 
#' estimated from the second derivative of the likelihood 
#' (i.e., the hessian).  If the likelihood is not 
#' differentiable, the variance-covariance matrix is 
#' a matrix of 0's that are intepreted as "pending" (i.e., 
#' pending bootstrapping). 
#' 
#' @inheritParams startLimits
#' @inheritParams print.dfunc
#' 
#' @return A square symmetric matrix estimating the 
#' variance-covariance matrix of parameters in \code{x}. 
#' Dimension of return is p X p, where p = \code{length(x$par)}.
#' 
#' @export
#' 
varcovarEstim <- function( x, ml ){

  nP <- length(x$par)
  nmsP <- names(x$par)
  
  if( !(ml$likelihood %in% differentiableLikelihoods()) ){
    # varcovar is PENDING bootstrapping ----
    varcovar <- NULL
  } else {
    # we want to estimate varcovar
    if( is.null(x$hessian) ){
      hessian <- Rdistance::secondDeriv(
          x = x$par
        , FUN = nLL
        , eps = getOption("Rdistance_hessEps")
        , ml = ml
      )
    } else {
      # Optim returns a hessian, others may, use it.
      hessian <- x$hessian
    }
  
    warn <- getOption("Rdistance_warn")
    if (x$convergence != 0) {
      if (warn) warning("Distance function did not converge, or converged to (Inf,-Inf)")
      varcovar <- matrix(NaN, nP, nP)
    } else if (!any(is.na(hessian)) & !any(is.infinite(hessian))){
      qrh <- base::qr(hessian)
      if (qrh$rank < nP) {
        if (warn) warning("Singular variance-covariance matrix.")
        varcovar <- matrix(NaN, nP, nP)
      } else {
        varcovar <- tryCatch(solve(hessian), error = function(e){NaN})
        if (length(varcovar) == 1 && is.nan(varcovar)){
          if (warn) warning("Singular variance-covariance matrix.")
          varcovar <- matrix(NaN, nP, nP)
        }
      }
    } else {
      if (warn) warning("Covariance matrix has Inf or NA elemetns.")
      varcovar <- matrix(NaN, nP, nP)
    }
    dimnames(varcovar) <- list(nmsP, nmsP)
  }
 
  # The other place varcovar gets estimated is in 'abundEstim'
  
  varcovar
  
}