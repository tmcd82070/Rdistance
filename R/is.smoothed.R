#' @title is.smoothed - Tests for smoothed distance functions
#' 
#' @description
#' Determines whether a distance function is a non-parametric
#' smooth or classic parameterized function.
#' 
#' @param x An Rdistance model frame or fitted distance function. 
#' 
#' @return TRUE if the model frame or fitted distance function 
#' arises from a non-parametric density smoother. FALSE if the 
#' model frame or distance 
#' function is a parameterized function. 
#' 
#' @export
#' 
is.smoothed <- function(x, ...){
  x$likelihood == "smu"
}
  