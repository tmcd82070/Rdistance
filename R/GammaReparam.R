#' @title Reparameterise Gamma parameters for use in dgamma
#' 
#' @description Transform Rdistance's version of the Gamma distribution
#' parameters, which is that of Becker and Quan, into the version for 
#' use in R::dgamma()
#' 
#' @param shp Rdistance's shape parameter
#' 
#' @param scl Rdistance's scale parameter.  This is the parameter that 
#' is function of covariates. 
#' 
#' @return a list with components $shp and $scl, which are the 
#' reparameterized versions of the input parameters suitable for 
#' us in R::dgamma().
#' 
GammaReparam <- function(shp, scl){

  b <- (1/gamma(shp)) * (((shp - 1)/exp(1))^(shp - 1))
  list(shp = shp
     , scl = scl * b)

}