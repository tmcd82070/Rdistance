#' @title Modes of the Gamma distribution
#' 
#' @description
#' Compute mode (i.e., maximum) of Rdistance's version of the gamma 
#' distribution. 
#' 
#' @param parms A matrix of gamma distribution parameters. First column 
#' is the scale parameter. Second column is the shape parameter. 
#' 
#' @return A vector of the location of the gamma modes. 
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