#' @title intercept.only - Detect intercept-only distance function
#' 
#' @description
#' Utility function to detect whether a distance function has 
#' covariates beyond the intercept. If the model contains an 
#' intercept-only, effective distance is constant across detections
#' and short-cuts can be implemented in code.
#' 
#' @inheritParams is.smoothed
#'
#' @return TRUE if \code{x} contains an intercept-only.
#' FALSE if \code{x} contains at least one detection-level 
#' or transect-level covariate in the detection function.
#' 
# NOT EXPORTED

intercept.only <- function(x){
  
  mt <- terms(x$mf)
  if( attr(mt,"intercept") == 0 ){
    # it has no intecept, so not "intecept only"
    return(FALSE)
  }
  
  if( length(attr(mt, "term.labels")) == 0 ){
    # It has an intercept but no other covariates
    return(TRUE)
  } else {
    # It has an intercept, but at least one other covariate
    return(FALSE)
  }
  
}