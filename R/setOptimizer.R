#' @title Set optimizing routine
#' 
#' @description
#' Checks that the maximum likelihood optimizing routine is appropriate 
#' for the requested likelihood function. That is, checks that gradient 
#' based maximization routines are only applied to smooth likelihoods, 
#' and that gradient-free methods are used for non-smooth likelihoods.
#' 
#' @inheritParams startLimits
#' 
#' @return If Rdistance options are changed, a list of the options and 
#' their original values.  The return can be used outside this routine 
#' to set options back to their state when this routine was entered.  
#' If no options changed, the return in NULL. 
#' 
#' @export
setOptimizer <- function(ml){
  
  out <- NULL
  optimizerAlgo <- getOption("Rdistance_optimizer")
  
  if( !(ml$likelihood %in% differentiableLikelihoods()) ){
    
    # Non-smooth likelihood case ----
    if( optimizerAlgo == "default" ){
      out <- c(out, options(Rdistance_optimizer = "optim_Nelder-Mead")) # default for non-smooth likelihoods
    } else {
      # check if optimizer is appropriate
      if( !(optimizerAlgo %in% c("optim_Nelder-Mead", "optim_SANN", "hookeJeeves")) ){
          stop(paste("Gradient based optimization method"
                     , optimizerAlgo
                     , "cannot be used because likelihood"
                     , ml$likelihood
                     , "is not smooth (i.e., differentiable)."
                     , "Use method 'optim_Nelder-Mead', 'optim_SANN', or 'hookeJeeves'."
          ))
        }
    }
    
    nInts <- getOption("Rdistance_intEvalPts")
    if(nInts < 301){
      # bump up integral points
      out <- c(out, options(Rdistance_intEvalPts = 301))
    }
    checkNEvalPts(getOption("Rdistance_intEvalPts")) # make sure coefs match
      
  } else {
    
    # Smooth likelihood case ----
    if( optimizerAlgo == "default" ){
      out <- c(out, options(Rdistance_optimizer = "nlminb")) # default for smooth likelihoods
    } 
      
    # Check whether problem is univariate and Hooke-Jeeves is called for; HJ can't do univariate problems ----
    termLabs <- attr(stats::terms(ml$formula), "term.labels")
    termLabs <- termLabs[!grepl("groupsize\\(", termLabs)]
    if( (length(termLabs) == 0) && 
        (getOption("Rdistance_optimizer") == "hookeJeeves") &&
        (ml$likelihood != "hazrate") ){
      stop(paste("Cannot estimate an intercept-only model using 'hookeJeeves'.",
                 "Reset optimizer with options(Rdistance_optimizer = 'nlminb'), or restart R",
                 "and re-attach Rdistance"))  
    } 
    
  }
  
  out
  
}