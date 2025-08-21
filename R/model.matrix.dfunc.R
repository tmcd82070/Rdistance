#' @title Rdistance model matrix
#'
#' @description 
#' Extract the model matrix ("X" matrix) from an Rdistance model object.
#'
#' @usage \method{model.matrix}{dfunc}(object, \dots)
#' 
#' @inheritParams predict.dfunc 
#' 
#' @param ... Ignored
#' 
#' @return A matrix containing covariates for fitting an
#' Rdistance model.
#' 
#' @examples
#' 
#' data(sparrowDf)
#' sparrowModel <- parseModel( sparrowDf, dist ~ observer )
#' model.matrix(sparrowModel)
#' 
#' @export
#' @importFrom stats model.matrix terms
#' 
model.matrix.dfunc <- function(object, ...){
  # ml$mf is a regular linear model frame, with terms, so this is easy
  stats::model.matrix(stats::terms(object$mf), object$mf)
}
