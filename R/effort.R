#' @title Effort information
#'
#' @description 
#' Extract effort information from an Rdistance data frame. Effort is 
#' length of line-transects or number of points on point-transects.
#' 
#' @inheritParams transectType
#' 
#' @param ... Ignored
#' 
#' @return A vector containing effort. If line-transects, return is 
#' length of transects, with units.  If point-transects, return is 
#' number of points (integers, no units).  Vector length is
#' number of transects. If input is not an RdistDf or estimated distance
#' function, return is NULL.
#' 
#' @examples
#' 
#' data(sparrowDf)
#' effort(sparrowDf)
#' fit <- dfuncEstim(sparrowDf, dist ~ 1)
#' effort(fit)
#' 
#' @export
#' 
effort <- function(x, ...){
  if( inherits(x, "dfunc") ){
    return( dplyr::pull(x$data, attr(x$data, "effortColumn")) ) # or x$data[[attr(x$data, "effortColumn")]]
  } else if( is.RdistDf(x, verbose = TRUE) ){
    return( dplyr::pull(x, attr(x, "effortCol")) )
  } else {
    return(NULL)
  }
}
