#' @title Type of observations
#' 
#' @description Return the type of observations (single or multiple observers) 
#' represented in either a fitted distance function or Rdistance data frame. 
#' 
#' @inheritParams transectType
#' 
#' @details 
#' This function is a simple helper function.  If `x` is an
#' estimated distance object, it polls the `obsType`
#' attribute of the object's Rdistance data frame. If `x` is an Rdistance nested data frame, it 
#' polls the `obsType` attribute. 
#' 
#' @return One of the following values: 
#' "single", "1given2", "2given1", or "both". If observation type 
#' has not been assigned, 
#' return is NULL.
#' 
#' @export
observationType <- function(x){
  if( inherits(x, "dfunc") ){
    return( attr(x$data, "obsType") )
  } else {
    return( attr(x, "obsType") )
  }
}
