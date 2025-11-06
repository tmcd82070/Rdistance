#' @export
#' 
#' @rdname unitHelpers
#' 
#' @returns For \code{dropUnits}, argument \code{x} with no units. If 
#' input \code{x} has no units, \code{x} is returned unchanged. 
#' 
dropUnits <- function(x){
  units::set_units(x, NULL)
}
