#' @export
#' 
#' @rdname unitHelpers
#' 
#' @returns For `dropUnits`, argument `x` with no units. If 
#' input `x` has no units, `x` is returned unchanged. 
#' 
dropUnits <- function(x){
  units::set_units(x, NULL)
}
