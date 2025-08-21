#' @title Detect intercept-only distance function
#' 
#' @description
#' Utility function to detect whether a distance function has 
#' covariates beyond the intercept. If the model contains an 
#' intercept-only, effective distance is constant across detections
#' and short-cuts can be implemented in code.
#' 
#' @inheritParams predict.dfunc
#'
#' @return TRUE if \code{object} contains an intercept-only.
#' FALSE if \code{object} contains at least one detection-level 
#' or transect-level covariate in the detection function.
#' 
# NOT EXPORTED

intercept.only <- function(object){
  
  mt <- terms(object$mf)
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
