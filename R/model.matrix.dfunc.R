#' @title model.matrix - Rdistance model matrix
#'
#' @description 
#' Extract the model matrix ("X" matrix) from an Rdistance model object.
#'
#' @usage \method{model.matrix}{dfunc}(ml, \dots)
#' 
#' @inheritParams distances 
#' 
#' @return A matrix containing covariates for fitting in an
#' Rdistance model.
#' 
#' @examples
#' 
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' model.matrix(sparrowModel)
#' 
#' @export
#' 
model.matrix.dfunc <- function(ml, ...){
  # object$mf is a regular linear model frame, with terms, so this is easy
  stats::model.matrix(stats::terms(ml$mf), ml$mf)
}
