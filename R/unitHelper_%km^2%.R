#' @rdname unitHelpers
#'
#' @usage x \%km^2\% .
#'
`%km^2%` <- function(x, .){
  units::set_units(x, "km^2")
}

