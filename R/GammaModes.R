#' @title Modes of the Gamma distribution
#' 
#' @description
#' Compute mode (i.e., maximum) of Rdistance's version of the gamma 
#' distribution. 
#' 
#' @param params A matrix of Gamma distribution parameters. First column 
#' is the scale parameter, and is a fuction of covariates.
#' Second column is the shape parameter. 
#' 
#' @return A vector of the locations of the gamma modes associated 
#' with each row in the \code{params} matrix. 
#' 
#' @export
GammaModes <- function( params ){
  
  scl = exp(params[,1])
  shp = params[,2]
  
  b <- GammaReparam( shp = shp, scl = scl )
  
  shp <- b$shp
  scl <- b$scl
  
  mod <- (shp - 1) * scl
  
  mod

}