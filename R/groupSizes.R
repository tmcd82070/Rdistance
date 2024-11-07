#' @title groupSizes - Group Sizes
#'
#' @description 
#' Extract the group size information from 
#' an Rdistance model frame.
#' 
#' @inheritParams startLimits
#' 
#' @param ... Ignored
#' 
#' @return A vector containing group sizes contained
#' in the Rdistance model frame or fitted object.
#' 
#' @examples
#' 
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' model.response(sparrowModel)
#' distances(sparrrowModel)
#' 
#' @export
#' 
groupSizes <- function(ml, ...){
  #ml$mf[, attr(terms(ml$mf), "offset")]
  stats::model.offset(ml$mf)
}
