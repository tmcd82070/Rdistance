#' @rdname unitHelpers
#' 
#' @usage x \%ft\% .
#' 
`%ft%` <- function(x, . = NULL){
  units::set_units(x, "ft")
}
