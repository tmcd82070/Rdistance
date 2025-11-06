#' @export
#' 
#' @rdname unitHelpers
#' 
setUnits <- function(x, u){
  units::set_units(x, u, mode = "standard")
}
