#' @rdname unitHelpers
#'
#' @usage x \%acre\% .
#'
`%acre%` <- function(x, . = NULL){
  units::set_units(x, "acre")
}
