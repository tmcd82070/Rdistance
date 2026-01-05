#' @title Reparameterise Gamma parameters for use in dgamma
#' 
#' @description Transform Rdistance's version of the Gamma distribution
#' parameters, which is that of Becker and Quan, into the version for 
#' use in R::dgamma() and elsewhere.
#' 
#' @param shp Rdistance's shape parameter
#' 
#' @param scl Rdistance's scale parameter.  This parameter 
#' is related to covariates via exp(x'B).
#' 
#' @return A list with components $shp and $scl, which are the 
#' re-parameterized versions of the input parameters suitable for 
#' us in R::dgamma().
#' 
#' @seealso 'Details' section of \code{\link{Gamma.like}}  
#' for Rdistance's Gamma distribution
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
#' # Gamma distribution with (Rd.scl, Rd.shp) from 0 to 100
#' curve(dgamma(x, shape=dgParams$shp, scale = dgParams$scl)
#'            , from = 0
#'            , to = 100)
#'  
#' # Rdistance's version: same curve but scaled so maximum = 1
#' x <- seq(0, 100, length = 200) 
#' scl <- dgParams$scl
#' shp <- dgParams$shp
#' m <- (shp - 1) * scl
#' g <- (x / m)^(shp - 1) * exp(-(x - m) / scl) # distance function
#' plot(x, g, type = "l")
#' 
#' @export
#' 
GammaReparam <- function(shp, scl){
  
  k <- (1/gamma(shp)) * (((shp - 1)/exp(1))^(shp - 1))
  list(shp = shp
       , scl = scl * k)
  
}