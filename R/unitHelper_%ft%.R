#' @rdname unitHelpers
#' 
#' @usage x \%ft\% .
#' 
`%ft%` <- function(x, .){
  units::set_units(x, "ft")
}
