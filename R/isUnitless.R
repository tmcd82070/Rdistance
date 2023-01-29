#' @title isUnitless - Test whether object is unitless
#' 
#' @description Tests whether a 'units' object is 
#' unitless.  If something is unitless, sometimes it is 
#' better to remove it's units before calculations. 
#' Unitless objects such as ratios sometimes get assigned
#' units of '[1]', which is preferable, and 
#' sometimes they have units like '[m/m]'.  The 
#' \code{units} package should convert '[m/m]' to 
#' '[1]', but it does not always. 
#' 
#' @param obj  A numeric scaler or vector, with or without units. 
#' 
#' @return TRUE if \code{obj} has units and they 
#' are either '[1]' or the denominator units equal
#' the numerator units.  Otherwise, return FALSE.
#' If \code{obj} does not have units, this routine
#' returns TRUE. 
#' 
#' @examples 
#' a <- units::set_units(2, "m")
#' b <- a / a
#' isUnitless(a)
#' isUnitless(b)
#' isUnitless(3)
#' 
#' @export
isUnitless <- function(obj){
  
  if(!is.numeric(obj)){
    stop("'obj' must be numeric.")
  }
  
  if( !inherits(obj, "units") ){
    # Missing units = unitless
    return(TRUE)
  } else if( units(obj) == units::unitless ){
    return(TRUE)
  } else if (isTRUE(all.equal(units(obj)$numerator, units(obj)$denominator))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  NA
}
