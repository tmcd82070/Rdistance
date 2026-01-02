#' @title Reparameterise Gamma parameters for use in dgamma
#' 
#' @description Transform Rdistance's version of the Gamma distribution
#' parameters, which is that of Becker and Quan, into the version for 
#' use in R::dgamma()
#' 
#' @param shp Rdistance's shape parameter
#' 
#' @param scl Rdistance's scale parameter.  This is the parameter that 
#' is related to covariates. 
#' 
#' @return a list with components $shp and $scl, which are the 
#' re-parameterized versions of the input parameters suitable for 
#' us in R::dgamma().
#' 
#' @examples
#' # Rdistance Gamma parameters
#' Rd.scl <- 50  # must be >0
#' Rd.shp <- 1.5 # must be >1
#' 
#' # dgamma parameters
#' dgParams <- GammaReparam(Rd.shp, Rd.scl)
#' dgParams
#' 
#' # Likelihood at (Rd.scl, Rd.shp) from 0 to 100
#' curve(dgamma(x, shape=dgParams$shp, scale = dgParams$scl)
#'            , from = 0
#'            , to = 100)
#' 
#' @export
#' 
GammaReparam <- function(shp, scl){

  b <- (1/gamma(shp)) * (((shp - 1)/exp(1))^(shp - 1))
  list(shp = shp
     , scl = scl * b)

}