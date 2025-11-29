#' @rdname unitHelpers
#'
#' @usage x \%mi\% .
#'
`%mi%` <- function(x, . = NULL){
  units::set_units(x, "mi")
}
