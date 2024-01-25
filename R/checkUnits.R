#' @title checkUnits - Check for the presence of units
#' 
#' @description
#' Check for the presence of physical measurement units in 
#' an \code{Rdistance} data frame. 
#' 
#' @param ml An \code{Rdistance} model list.  This is a list
#' of parameters for a distance model and is produced by 
#' \code{\link{parseModel}}.  
#' 
#' @returns The input \code{ml} list, with units of various 
#' quantities converted to common units.  If a check fails, for example, 
#' a quantity does not have units, an error is thrown.
#' 
#' @export
#' 
checkUnits <- function(ml){

    # Units for dist ----
    if ( !inherits(ml$dist, "units") & ml$control$requireUnits ){
        dfName <- deparse(substitute(ml$dataName))
        respName <- as.character(attr(ml$mt, "variables"))[attr(mt, "response") + 1]
        stop(paste("Measurement units required on distances.",
            "Assign units by attaching 'units' package then:\n",
            paste0("units(",dfName,"$", respName, ")"), "<- '<units of measurment>',\n",
            "for example 'm' (meters) or 'ft' (feet). See units::valid_udunits()"))
    } else if ( ml$control$requireUnits ){
        # if we are here, dist has units
        # set units for output by converting dist units; 
        # w.lo, w.hi, and x.scl will all be converted later
        if ( !is.null(ml$outputUnits) ){
            ml$dist <- units::set_units(ml$dist, ml$outputUnits, mode = "standard")
        } else {
            ml$outputUnits <- units(ml$dist)
        }
    }
    
    # Units for w.lo ----
    if ( !inherits(ml$w.lo, "units") & ml$control$requireUnits ){
        if ( ml$w.lo[1] != 0 ){
            stop(paste("Measurment units required on minimum distance (w.lo).",
                "Assign units by attaching 'units' package then:\n",
                "units(w.lo) <- '<units>' or",
                paste0("units::as_units(", w.lo,", <units>) in function call\n"),
                "See units::valid_udunits() for valid symbolic units."))
        }
        # if we are here, w.lo is 0, it has no units, and we require units
        ml$w.lo <- units::set_units(ml$w.lo, ml$outputUnits, mode = "standard") # assign units to 0
    } else if ( ml$control$requireUnits ){
        # if we are here, w.lo has units and we require units, convert to the output units
        # Technically, I don't think we need to do this.  As long as w.lo has units, we are good.
        # We do this here so that we don't do it later when we print units during output.
        ml$w.lo <- units::set_units(ml$w.lo, ml$outputUnits, mode = "standard")
    }
    
    # Units on w.hi ----
    if (is.null(ml$w.hi)){
        ml$w.hi <- max(ml$dist, na.rm = TRUE) # units flow through max() automatically
    } else if ( !inherits(ml$w.hi, "units") & ml$control$requireUnits ){
        stop(paste("Max distance measurement units are required.",
            "Assign units by attaching 'units' package then:\n",
            "units(w.hi) <- '<units>' or",
            paste0("units::as_units(", w.hi,", <units>) in function call\n"),
            "See units::valid_udunits() for valid symbolic units."))
    } else if ( ml$control$requireUnits ){
        # if we are here, w.hi has units and we require them, convert to output units
        # Again, technically I don't think we need to do this.  Happens automatically in computations
        ml$w.hi <- units::set_units(ml$w.hi, ml$outputUnits, mode = "standard")
    }
    
    ml

}