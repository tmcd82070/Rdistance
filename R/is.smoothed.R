#' @title Tests for smoothed distance functions
#' 
#' @description
#' Determines whether a distance function is a non-parametric
#' smooth or classic parameterized function.
#' 
#' @inheritParams predict.dfunc
#' 
#' @return TRUE if the model frame or fitted distance function 
#' arises from a non-parametric density smoother. FALSE if the 
#' model frame or distance 
#' function is a parameterized function. 
#' 
#' @export
#' 
is.smoothed <- function(object){
  object$likelihood == "smu"
}
  
