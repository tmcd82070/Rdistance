#' @title is.points - Tests for point surveys
#' 
#' @description
#' Determines whether a distance function is for a point survey 
#' or line survey. 
#' 
#' @inheritParams transectType
#' 
#' @return TRUE if the model frame or fitted distance function 
#' contains point surveys.  FALSE if the model frame or distance 
#' function contains line transect surveys. 
#' 
#' @export
#' 
is.points <- function(x){
  typ <- Rdistance::transectType(x)
  if(!is.null(typ)){
    return(typ == "point")
  } else {
    stop(paste("'transType' component of object x missing.  Cannot determine"
               , "whether data is from point or line transect surveys."))
  }
}
  