#' @title triangle.like - Triangular distance function
#' 
#' @description 
#' Computes the triangular likelihood for use as a distance function. 
#' The triangular likelihood has constant downward slope to a maximum. 
#' 
#' @param a The maximum value of the triangular likelihood. 
#' 
#' @return Vector of triangular likelihood values that integrate to 1.0
#' 
#' @examples
#' 
#' x <- 0:100
#' y <- triange.like( 80, x )
#' 
#' @export
#' 
triangular.like <- function(a, dist, covars=NULL, 
                            pointSurvey=FALSE, w.lo=0, w.hi, 
                            series="", expansions=0, scale=TRUE){
  L <- (2/a)*(1 - dist/a)
  L[ L < 0 ] <- 0
  L
}