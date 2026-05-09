#' @rdname unitHelpers
#'
#' @usage x \%yd\% .
#'
`%yd%` <- function(x, . = NULL){
  units::set_units(x, "yd")
}
