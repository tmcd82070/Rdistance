#' @rdname unitHelpers
#'
#' @usage x \%inches\% .
#'
`%inches%` <- function(x, .){
  units::set_units(x, "in")
}
