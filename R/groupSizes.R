#' @title Group Sizes
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
#' data("sparrowDf")
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' stats::model.offset(sparrowModel$mf)
#' groupSizes(sparrowModel)  # same, but future-proof
#'
#' sparrowModel <- parseModel( sparrowDf
#'                  , dist ~ observer + groupsize(groupsize) )
#' groupSizes(sparrowModel)  
#' 
#' @export
#' 
groupSizes <- function(ml, ...){
  stats::model.offset(ml$mf)
}
