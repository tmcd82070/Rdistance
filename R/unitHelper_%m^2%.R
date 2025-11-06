#' @rdname unitHelpers
#'
#' @usage x \%m^2\% .
#'
`%m^2%` <- function(x, y = NULL){
  units::set_units(x, "m^2")
}
