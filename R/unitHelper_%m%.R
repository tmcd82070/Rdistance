#' @usage x \%m\% .
#'
#' @param . Placeholder for the fixed unit assignment operators. Ignored.
#' See Details.
#' 
#' @details
#' The fixed unit assignment operators are designed to behave somewhat like 
#' unary operators (i.e., 1 argument);
#' but, R does not allow 
#' user defined unary operators. 
#' Technically, the fixed unit assignment operators are instances of
#' R's user-defined infix 
#' operator, and as such they require two arguments. 
#' Their syntax must be 
#' \code{x \%<units>\% <something>}; but, the second argument is ignored
#' and '.' is suggested. See Examples.  
#' 
#' @returns 
#' For all the fixed unit assignment operators (i.e., \%<units>\%), 
#' argument x with the respective units assigned. 
#' 
#' 
#' @examples 
#' 
#' # For fixed unit assignment, 2nd argument does not matter
#' # All of the following are equivalent
#' 2 %m%.
#' 2 %m% x
#' 2 %m% 3
#' 2 %m% NULL
#' 2 %m% NA
#' 
#' # Conversion:
#' x <- 10 %#% "ft"
#' x %m%.
#' 
#' @exportPattern `%.+%`
#' 
#' @rdname unitHelpers
#' 
`%m%` <- function(x, . = NULL){
  units::set_units(x, "m")
}
