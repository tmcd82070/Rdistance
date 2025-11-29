#' @rdname unitHelpers
#'
#' @usage x \%m^2\% .
#'
`%m^2%` <- function(x, . = NULL){
  units::set_units(x, "m^2")
}
