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
  if(!is.null(x$transType)){
    return(x$transType == "point")
  } else {
    stop(paste("'transType' component of object x missing.  Cannot determine"
               , "whether data is from point of transect surveys."))
  }
}
  