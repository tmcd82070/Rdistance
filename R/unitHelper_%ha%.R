#' @rdname unitHelpers
#'
#' @usage x \%ha\% .
#'
`%ha%` <- function(x, y = NULL){
  units::set_units(x, "ha")
}
