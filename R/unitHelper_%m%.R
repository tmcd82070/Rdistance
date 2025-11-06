#' @usage x \%m\% .
#'
#' @param . Placeholder for the fixed unit assignment operators. Technically, 
#' this is the second argument to the user-defined 
#' infix operator \%<units>\%. This argument must be present, but is ignored 
#' (see Details and Examples). Used to trick \%<units>\% into behaving 
#' somewhat like a unary operator.
#' 
#' @details
#' Ideally, the fixed unit assignment operators  (i.e., \%<units>\%) 
#' would behave like true unary operators. R does not allow 
#' user defined unary operators, and technically these functions 
#' are instances of R's user-defined infix 
#' operator. As such, something must follow the second \%. Their syntax must be 
#' \code{x \%<units>\% <something>} rather than simply \code{x \%<units>\%}.
#' Whatever comes after the second \% (technically, the second argument to 
#' the infix operator) 
#' does not matter and '.' is suggested. See Examples. 
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
`%m%` <- function(x, .){
  units::set_units(x, "m")
}
