#' @rdname unitHelpers
#'
#' @usage x \%yd\% .
#'
`%yd%` <- function(x, y = NULL){
  units::set_units(x, "yd")
}
