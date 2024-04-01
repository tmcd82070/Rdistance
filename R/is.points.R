#' @title is.points - Tests for point surveys
#' 
#' @description
#' Determines whether a distance function is for a point survey 
#' or line survey. 
#' 
#' @inheritParams is.smoothed
#' 
#' @return TRUE if the model frame or fitted distance function 
#' contains point surveys.  FALSE if the model frame or distance 
#' function contains line transect surveys. 
#' 
#' @export
#' 
is.points <- function(x, ...){
  x$transType == "point"
}
  