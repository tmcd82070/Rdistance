#' @title Estimate g(0) or g(x)
#' 
#' @description Estimate distance function scaling factor
#' , g(0) or g(x), for a specified distance function.
#' 
#' @param fit An estimated `dfunc` object.  See `dfuncEstim`.
#'   
#' @details This routine scales sightability such that 
#' g(`x.scl`) = `g.x.scl`, where g() is the sightability function.
#' Specification of `x.scl` and `g.x.scl` covers several estimation cases:
#' \enumerate{  
#'   \item **g(0) = 1** : (the default) Inputs are `x.scl` = 0, `g.x.scl` = 1. 
#'   If `w.lo` > 0, `x.scl` will be set to `w.lo`
#'   so technically this case is g(`w.low`) = 1.
#'   
#'   \item **User supplied probability at specified distance**: Inputs are 
#'   `x.scl` = a number greater than or equal 
#'   to `w.lo`, `g.x.scl` = a number between 0 and 1.  This case 
#'   covers situations where sightability on the transect (distance 0) is 
#'   not perfect.  This case  assumes researchers have an independent 
#'   estimate of sightability at distance 
#'   `x.scl` off the transect.  For example, researchers could be
#'   using multiple 
#'   observers to estimate that sightability at distance `x.scl` 
#'   is `g.x.scl`. 
#'   
#'   \item **Maximum sightability specified**: Inputs 
#'   are `x.scl`="max", `g.x.scl` =  a number 
#'   between 0 and 1.  In this case, 
#'   g() is scaled such that its maximum value is `g.x.scl`.  
#'   This routine computes the distance at which g() is maximum, sets 
#'   g()'s height there to `g.x.scl`, and returns `x.max` where 
#'   x.max is the distance at which g is maximized. This case covers the 
#'   common aerial survey situation where maximum sightability is slightly 
#'   off the transect, but the distance at which the maximum occurs 
#'   is unknown. 
#'   
#'   \item **Double observer system**: Inputs are 
#'   `x.scl`="max", `g.x.scl` = <a data frame>. 
#'   In this case, g(*x*) = *h*, where *x* is the distance that 
#'   maximizes g and *h* is the height of g() at *x* 
#'   computed from the double observer data frame (see below for 
#'   structure of the double observer data frame).
#'   
#'   
#'   \item **Distance of independence specified, height computed from double 
#'   observer system**: Inputs are 
#'   `x.scl` = a number greater than or equal to `w.lo`
#'   `g.x.scl` = a data frame.  In this case, g(`x.scl`) = *h*, 
#'   where *h* is computed from the double observer data frame 
#'   (see below for structure of the double observer data frame). 
#'
#' }   
#'   
#'   When `x.scl`, `g.x.scl`, or `observer` are NULL, the routine 
#'   will look for and use `$call.x.scl`, or `$call.g.x.scl`, or 
#'   `$call.observer` components of the `fit` object for whichever 
#'   of these three parameters is missing.  Later, different 
#'   values can be specified in a direct call to `F.gx.estim` 
#'   without having to re-estimate the distance function. Because of this feature, 
#'   the default values in `dfuncEstim` are `x.scl` = 0 and 
#'   `g.x.scl` = 1 and `observer` = "both". 
#'   
#' @section Structure of the double observer data frame:  
#' When `g.x.scl` is a data frame, it is assumed to contain 
#'   the components `$obsby.1` and `$obsby.2` (no flexibility on names). 
#'   Each row in the data frame contains data from one sighted target. 
#'   The `$obsby.1` and `$obsby.2` components are 
#'   TRUE/FALSE (logical) vectors indicating whether 
#'   observer 1 (`obsby.1`) or observer 2 (`obsby.2`) spotted the target.
#'   
#' @return A list comprised of the following components:
#'   \item{x.scl}{The value of x (distance) at which g() is evaluated.  }
#'   \item{comp2 }{The estimated value of g() when evaluated at `x.scl`.  }
#'   
#' 
#' @seealso [dfuncEstim()]
#' 
#' @examples 
#' 
#' data(sparrowDf)
#' fit <- dfuncEstim(sparrowDf, dist ~ groupsize(groupsize))
#' gxEstim(fit)
#'   
#' fit <- dfuncEstim(sparrowDf, dist ~ groupsize(groupsize)
#'                 , x.scl = 50 %m%.
#'                 , g.x.scl = 0.75)
#' gxEstim(fit)
#' plot(fit)
#' abline(h=0.75)
#' abline(v=50%m%.)
#'   
#' @export

gxEstim <- function( fit ){

  #   Will need measurement units later ----
  distUnits <- fit$outputUnits
  
  #   Compute x (the point to evaluate g() at) ----
  # if( is.null( fit$x.scl ) ){
  #     x.scl <- fit$call.x.scl
  # }
  
  #   Compute height of g() at x.scl ----
  # if( is.null( g.x.scl ) ){
  #     g.x.scl <- fit$call.g.x.scl
  # }
  
  #   Double observers? ----
  # if( is.null( observer ) ){
  #     observer <- fit$call.observer
  # }
  
  x.scl <- fit$x.scl
  
  if( is.character(x.scl) ){
    if( x.scl == "max" ){
      if( fit$likelihood %in% c("halfnorm", "negexp", "hazrate", "triangle", "huber") & 
                 fit$expansions == 0 ){
        # All these are monotonically negative
        x.scl <- fit$w.lo 
      } 
    } else {
        # x.scl != "max"
        x.scl <- setUnits(NA, fit$outputUnits)
        warning("Invalid character string for x.scl specified in gxEstim. x.scl set to missing.")
    }
  } 

  #   --------------------------------------------------------------------------------------
  #   Now compute g(x)
  
  if( Rdistance::observationType(fit) != "single" ){
      #   Compute g(x) from double observer data
      stop("Double observer systems not implemented.")
      #g.x.scl <- doubleObsProb( fit )
  } else {
      # Single observer; but, g(x) could be less than 1
      # g(x) is specified, nothing to do except check validity
      if( length(fit$g.x.scl) > 1 ){
          g.x.scl <- fit$g.x.scl[1]
          warning(paste("Vector of g(x) values found in gxEstim. Only the first value,", g.x.scl, "has been used."))
      }
      if( fit$g.x.scl < 0 ){
          g.x.scl <- 0
          warning("Impossible g.x.scl < 0 specified in gxEstim. g(x) has been reset to 0.")
      } else if( fit$g.x.scl > 1 ){
          g.x.scl <- 1
          warning("Impossible g(x) > 1 specified in gxEstim. g(x) reset to 1.")
      } else if( is.character(fit$g.x.scl) ){
          stop(paste0("g.x.scl cannot be character valued. Found '", g.x.scl, "'. Convert this to numeric."))
      } else {
          g.x.scl <- fit$g.x.scl
      }
  }

  list( x.scl = x.scl, g.x.scl = g.x.scl )
}
