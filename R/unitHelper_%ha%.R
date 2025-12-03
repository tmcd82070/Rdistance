#' @rdname unitHelpers
#'
#' @usage x \%ha\% .
#'
`%ha%` <- function(x, . = NULL){
  units::set_units(x, "ha")
}
