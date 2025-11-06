#' @rdname unitHelpers
#'
#' @usage x \%cm\% .
#' 
`%cm%` <- function(x, .){
  units::set_units(x, "cm")
}
