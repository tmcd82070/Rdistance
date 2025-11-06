#' @rdname unitHelpers
#'
#' @usage x \%acre\% .
#'
`%acre%` <- function(x, y = NULL){
  units::set_units(x, "acre")
}
