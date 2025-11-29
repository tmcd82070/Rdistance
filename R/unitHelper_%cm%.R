#' @rdname unitHelpers
#'
#' @usage x \%cm\% .
#' 
`%cm%` <- function(x, . = NULL){
  units::set_units(x, "cm")
}
