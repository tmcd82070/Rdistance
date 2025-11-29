#' @rdname unitHelpers
#'
#' @usage x \%km\% .
#'
`%km%` <- function(x, . = NULL){
  units::set_units(x, "km")
}
