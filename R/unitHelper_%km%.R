#' @rdname unitHelpers
#'
#' @usage x \%km\% .
#'
`%km%` <- function(x, .){
  units::set_units(x, "km")
}
