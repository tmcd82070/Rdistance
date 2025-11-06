#' @rdname unitHelpers
#'
#' @usage x \%mi^2\% .
#'
`%mi^2%` <- function(x, y = NULL){
  units::set_units(x, "mi^2")
}
