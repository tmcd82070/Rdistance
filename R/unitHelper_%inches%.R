#' @rdname unitHelpers
#'
#' @usage x \%inches\% .
#'
`%inches%` <- function(x, . = NULL){
  units::set_units(x, "in")
}
