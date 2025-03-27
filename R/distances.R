#' @title distances - Observation distances
#'
#' @description 
#' Extract the observation distances (i.e., responses for 
#' an Rdistance model) from an Rdistance model frame.
#' 
#' @inheritParams startLimits
#' 
#' @param ... Ignored
#' 
#' @param na.rm Whether to include or exclude missing distance values. 
#' In \code{ml}, the model list containing the model frame, missing 
#' values of the response (distance) are potentially present for two 
#' reasons: (1) they are outside the strip w.lo to w.hi, and (2) they
#' are missing because the crew did not get a distance for that observation.
#' 
#' 
#' @return A vector containing observation distances contained
#' in the Rdistance model frame.
#' 
#' @examples
#' 
#' data(sparrowDf)
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' stats::model.response(sparrowModel$mf)
#' distances(sparrowModel) # same, but future-proof
#' 
#' @export
#' 
distances <- function(ml, na.rm = TRUE, ...){
  # ml$mf is a regular linear model frame, with terms, so this is easy
  y <- stats::model.response(ml$mf)
  
  # There could be missing distances b/c crew could not measure it or b/c
  # the obs is outside the strip.
  # We want to count this observation for abundance, but not distance function estimation
  if( na.rm ){
    y <- y[!is.na(y)]
  }
  y
}
