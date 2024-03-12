#' @title checkUnits - Check for the presence of units
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
  if ( !is.null(ml$outputUnits) ){
    ml$data <- ml$data |> 
      dplyr::mutate( !!ml$respName := units::set_units(dist, ml$outputUnits, mode = "standard"))
  } else {
      ml$outputUnits <- units(dist)
  }

  # Units for w.lo ----
  if ( !inherits(ml$w.lo, "units") ){
      if ( is.null(ml$w.lo) || (ml$w.lo[1] != 0) ){
          stop(paste("Measurement units required on minimum distance (w.lo).",
              "Assign units by attaching 'units' package then:",
              "units(w.lo) <- '<units>' or",
              paste0("units::set_units(", ml$w.lo,", <units>) in function call."),
              "See units::valid_udunits() for valid symbolic units."))
      }
      # if we are here, w.lo is 0, it has no units, and we require units
      ml$w.lo <- units::set_units(ml$w.lo, ml$outputUnits, mode = "standard") # assign units to 0
  } 
  # if we are here, w.lo has units and we require units, convert to the output units
  # Technically, I don't think we need to do this.  As long as w.lo has units, we are good.
  # We do this here so that we don't do it later when we print units during output.
  ml$w.lo <- units::set_units(ml$w.lo, ml$outputUnits, mode = "standard")

  # Units on w.hi ----
  if (is.null(ml$w.hi)){
    ml$w.hi <- max(dist, na.rm = TRUE) # units flow through max() automatically
  } else if ( !inherits(ml$w.hi, "units") ){
      stop(paste("Measurement units required on maximum distance (w.hi).",
          "Assign units by attaching 'units' package then:",
          "units(w.hi) <- '<units>' or",
          paste0("units::set_units(", ml$w.hi,", <units>) in function call."),
          "See units::valid_udunits() for valid symbolic units."))
  } 
  # if we are here, w.hi has units and we require them, convert to output units
  # Again, technically I don't think we need to do this.  Happens automatically in computations
  ml$w.hi <- units::set_units(ml$w.hi, ml$outputUnits, mode = "standard")

  # add checks for x.scl

  ml

}