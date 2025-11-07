#' @title Check for the presence of units
#' 
#' @description
#' Check for the presence of physical measurement units on key
#' columns of  
#' an \code{RdistDf} data frame. 
#' 
#' @param ml An \code{Rdistance} model list produced by 
#' \code{\link{parseModel}} containing a list
#' of parameters for the distance model. 
#' 
#' @returns The input \code{ml} list, with units of various 
#' quantities converted to common units.  If a check fails, for example, 
#' a quantity does not have units, an error is thrown.
#' 
#' @export
#' 
checkUnits <- function(ml){

  # If we are here, we assume that requireUnits == TRUE
  
  # Units for dist ----
  dist <- dplyr::pull(ml$data, ml$respName)
  if ( !inherits(dist, "units") ){
    stop(paste("Measurement units required on distances.",
          "Assign units by attaching 'units' package then:\n",
          paste0("units(",ml$dataName,"$", ml$respName, ")"), "<- '<units of measurment>',\n",
          "for example 'm' (meters) or 'ft' (feet). See units::valid_udunits()"))
  } 
  # if we are here, dist has units
  # set units for output by converting dist units; 
  # w.lo, w.hi, and x.scl will all be converted later
  # We want outputUnits to be units(dist), not e.g., string "m"
  if ( !is.null(ml$outputUnits) ){
    ml$data <- ml$data |> 
      dplyr::mutate(dplyr::across(.cols = ml$respName
                                , .fns = ~ setUnits(.x, ml$outputUnits)))
    ml$outputUnits <- units(setUnits(1, ml$outputUnits))
  } else {
    ml$outputUnits <- units(dist)
  }

  # Units for w.lo ----
  if ( !inherits(ml$w.lo, "units") ){
      if ( is.null(ml$w.lo) || (ml$w.lo[1] != 0) ){
          stop(paste("Measurement units required on minimum distance (w.lo).",
              "Assign units with statement like",
              paste0("setUnits(", 
                     ifelse(is.function(ml$w.lo), "<value>", ml$w.lo),
              ", <units>)."),
              "See 'help(unitHelpers)'."))
      }
      # if we are here, w.lo is 0, it has no units, and we require units
      ml$w.lo <- setUnits(ml$w.lo, ml$outputUnits) # assign units to 0
  } 
  # if we are here, w.lo has units and we require units, convert to the output units
  # Technically, I don't think we need to do this.  As long as w.lo has units, we are good.
  # We do this here so that we don't do it later when we print units during output.
  ml$w.lo <- setUnits(ml$w.lo, ml$outputUnits)

  # Units on w.hi ----
  if (is.null(ml$w.hi)){
    ml$w.hi <- max(dist, na.rm = TRUE) # units flow through max() automatically
  } else if ( !inherits(ml$w.hi, "units") ){
    stop(paste("Measurement units required on maximum distance (w.hi).",
               "Assign units with statement like",
               paste0("setUnits(", 
                      ifelse(is.function(ml$w.hi), "<value>", ml$w.hi),
                      ", <units>)."),
               "See 'help(unitHelpers)'."))
  } 
  # if we are here, w.hi has units and we require them, convert to output units
  # Again, technically I don't think we need to do this.  Happens automatically in computations
  ml$w.hi <- setUnits(ml$w.hi, ml$outputUnits)

  # Units on x.scl ---- 
  if( !inherits(ml$x.scl, "units") ){
    if( (ml$x.scl[1] != 0) && (ml$x.scl[1] != "max")){
      stop(paste("Measurement units for x.scl are required.",
                 "Assign units with statement like",
                 paste0("setUnits(", 
                        ifelse(is.function(ml$x.scl), "<value>", ml$x.scl),
                        ", <units>)."),
                 "See 'help(unitHelpers)'."))
    }
    # if we are here, x.scl is either 0 (w/o units) or "max"
    if(!is.character(ml$x.scl)){
      ml$x.scl <- setUnits(ml$x.scl, ml$outputUnits)
    }
  } else {
    # if we are here, x.scl has units, is not "max", so we convert to the output units
    ml$x.scl <- setUnits(ml$x.scl, ml$outputUnits)
  }

  ml

}
