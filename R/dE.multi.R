#' @title dE.multi - Estimate multiple-observer line-transect distance functions
#' 
#' @description Fits a detection function to off-transect 
#' distances collected by multiple observers. 
#' 
#' @inheritParams dfuncEstim
#' @inheritParams dE.single
#' 
#' @inherit dE.single return
#' 
#' @export
#' 
dE.multi <- function( data
                         , formula
                         , likelihood = "halfnorm"
                         , w.lo = units::set_units(0,"m")
                         , w.hi = NULL
                         , expansions = 0
                         , series = "cosine"
                         , x.scl = units::set_units(0,"m")
                         , g.x.scl = 1
                         , warn = TRUE
                         , outputUnits = NULL){
  
  
  # The double-observer method hasn't been checked since updating to version 2.x
  # It's likely that an error would be raised if a user did try the double-observer option,
  # but I'm adding a warning here, just in case
    stop("The double-observer routines have not been tested in Rdistance
          versions >2.x, so they have been disabled for the time being.
          Contact the Rdistance authors if you need double observer analyses.")

}