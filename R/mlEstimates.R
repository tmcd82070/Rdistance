#' @title Distance function maximum likelihood estimates
#' 
#' @description
#' Estimate parameters of a distance function using maximum likelihood.
#' 
#' @inheritParams startLimits
#' 
#' @param strt.lims A list containing start, low, and high limits for 
#' parameters of the requested likelihood. This list is typically produced
#' by a call to \code{\link{startLimits}}.
#' 
#' @return An Rdistance fitted model object. This object contains the 
#' raw object returned by the optimization routine (e.g., \code{nlming}), 
#' and additional components specific to Rdistance.
#' 
#' @export

mlEstimates <- function( ml
                       , strt.lims
                       ){

  # Missing observations: 
  #  (1) there can be missing responses (distances); but, Rdistance::distances drops them
  #      i.e., model.response(ml$mf) has missings (potentially), but Rdistance::distances(ml) does not
  #  (2) There are missing group sized in the model frame for transects with missing lengths. 
  #      These don't matter here.
  
  optimFunc <- getOption("Rdistance_optimizer")

  fit <- switch( optimFunc
      , "optim" = Optim(ml, strt.lims)
      , "nlminb" = Nlminb(ml, strt.lims)
      , "hookeJeeves" = HookeJeeves(ml, strt.lims)
      ,  stop(paste("Unknown optimizer function. Found", optimFunc))
  )
  
  fit 

}
