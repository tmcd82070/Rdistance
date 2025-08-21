#' @title Type of transects
#' 
#' @description Return the type of transects represented in either a fitted 
#' distance function or Rdistance data frame. 
#' 
#' @param x Either an estimated distance function, output by 
#' \code{dfuncEstim}, or an Rdistance nested data frame, output by 
#' \code{RdistDf}. 
#' 
#' @details 
#' This function is a simple helper function.  If \code{x} is an
#' estimated distance object, it polls the \code{transType} attribute 
#' of \code{x}'s Rdistance nested data frame. 
#' If \code{x} is an Rdistance nested data frame, it 
#' polls the \code{transType} attribute. 
#' 
#' @return A scalar. Either 'line' if \code{x} contains  
#' continuous line-transect detections, or 'point' if \code{x} contains 
#' point-transects detections. If transect type has not been assigned, 
#' return is NULL.
#' 
#' @export
transectType <- function(x){
  if( inherits(x, "dfunc") ){
    return( attr(x$data, "transType" ))
  } else {
    return( attr(x, "transType") )
  }
}
