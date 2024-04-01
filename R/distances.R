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
#' @return A vector containing observation distances in the 
#' the Rdistance model frame.
#' 
#' @examples
#' 
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' model.response(sparrowModel)
#' 
#' @export
#' 
distances <- function(ml, ...){
  # object$mf is a regular linear model frame, with terms, so this is easy
  stats::model.response(ml$mf)
}
