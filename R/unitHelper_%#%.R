#' @title Unit assignment helpers
#' 
#' @description
#' Helper functions for assigning physical measurement units in Rdistance. 
#' All are convenience wrappers for \code{units::}\code{\link[units]{set_units}}. 
#' 
#' @param x A numeric vector or matrix.  
#' 
#' @param u A string representing physical measurement units to 
#' assign to \code{x}, e.g., "m", "km", "m^2".  Valid units are  
#' listed in columns "(symbol|name)" of \code{\link[units]{valid_udunits}}.
#' 
#' @returns For \%#\% and \code{setUnits}, argument \code{x} with 
#' units \code{u} attached. 
#' 
#' @examples 
#' 
#' 2 %#% "m" 
#' setUnits(2,"km")
#' x <- 2 %#% "km^2"
#' 10 %#% units(x)
#' 2 %#% "km^2" %#% "acres" # Convert km^2 to acres
#' x %#% "acres"            # Same
#' x %#% NULL    # Drop units
#' dropUnits(x)  # Same
#' 
#' # %#%'s precedence is same as "^"; above "+" and "*"
#' # The following fails: 
#' # 2 %#% "m" / (2 %#% "ha") %#% "in/acre"
#' # The following succeeds:
#' (2 %#% "m" / (2 %#% "ha")) %#% "in/acre"
#' 
#' @export
#' 
#' @rdname unitHelpers
#' 
'%#%' <- function(x, u){
  units::set_units(x, u, mode = "standard")
}