#' @rdname unitHelpers
#'
#' @usage x \%mi\% .
#'
`%mi%` <- function(x, .){
  units::set_units(x, "mi")
}
