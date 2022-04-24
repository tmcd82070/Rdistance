#' @title F.gx.estim - Estimate g(0) or g(x)
#' 
#' @description Estimate g(0) or g(x) for a specified distance function.
#' 
#' @param fit An estimated \code{dfunc} object.  See \code{dfuncEstim}.
#' 
#' @param x.scl The x coordinate (a distance) at which to scale the 
#'   sightability function to \code{g.x.scl}, or the string "max".  
#'   When \code{x.scl} is specified (i.e., not 0 or "max"), it must have measurement 
#'   units assigned using either \code{library(units);units(x.scl) <- '<units>'}
#'   or \code{x.scl <- units::set_units(x.scl, <units>)}. See
#'   \code{units::valid_udunits()} for valid symbolic units. See 
#'   Details for more on 
#'   scaling the sightability function.
#'   
#' @param g.x.scl Height of the distance function at coordinate \emph{x}. 
#'   The distance function 
#'   will be scaled so that g(\code{x.scl}) = \code{g.x.scl}. 
#'   If \code{g.x.scl} is not 
#'   a data frame, it must be a numeric value (vector of length 1) 
#'   between 0 and 1. 
#'   See Details. 
#'   
#' @param observer A numeric scalar or text string specifying whether observer 1 
#'   or observer 2 or both were full-time observers. 
#'   This parameter dictates which set of observations form the denominator 
#'   of a double observer system.   
#'   If, for example, observer 2 was a data recorder and part-time observer, 
#'   or if observer 2 was the pilot, set \code{observer} = 1.  
#'   If \code{observer} = 1, observations by observer 1 not seen 
#'   by observer 2 are ignored. The estimate of detection in this case is the 
#'   ratio of number of targets seen by both observers 
#'   to the number seen by both plus the number seen by just observer 2. 
#'   If observer = "both", the 
#'   computation goes both directions.
#'   
#' @details This routine scales sightability such that 
#' g(\code{x.scl}) = \code{g.x.scl}, where g() is the sightability function.
#' Specification of \code{x.scl} and \code{g.x.scl} covers several estimation cases:
#' \enumerate{  
#'   \item \bold{g(0) = 1} : (the default) Inputs are \code{x.scl} = 0, \code{g.x.scl} = 1. 
#'   If \code{w.lo} > 0, \code{x.scl} will be set to \code{w.lo}
#'   so technically this case is g(\code{w.low}) = 1.
#'   
#'   \item \bold{User supplied probability at specified distance}: Inputs are 
#'   \code{x.scl} = a number greater than or equal 
#'   to \code{w.lo}, \code{g.x.scl} = a number between 0 and 1.  This case 
#'   covers situations where sightability on the transect (distance 0) is 
#'   not perfect.  This case  assumes researchers have an independent 
#'   estimate of sightability at distance 
#'   \code{x.scl} off the transect.  For example, researchers could be
#'   using multiple 
#'   observers to estimate that sightability at distance \code{x.scl} 
#'   is \code{g.x.scl}. 
#'   
#'   \item \bold{Maximum sightability specified}: Inputs 
#'   are \code{x.scl}="max", \code{g.x.scl} =  a number 
#'   between 0 and 1.  In this case, 
#'   g() is scaled such that its maximum value is \code{g.x.scl}.  
#'   This routine computes the distance at which g() is maximum, sets 
#'   g()'s height there to \code{g.x.scl}, and returns \code{x.max} where 
#'   x.max is the distance at which g is maximized. This case covers the 
#'   common aerial survey situation where maximum sightability is slightly 
#'   off the transect, but the distance at which the maximum occurs 
#'   is unknown. This case is the default, with \code{g.x.scl} = 1, 
#'   when gamma distance functions are estimated. 
#'   
#'   \item \bold{Double observer system}: Inputs are 
#'   \code{x.scl}="max", \code{g.x.scl} = <a data frame>. 
#'   In this case, g(\emph{x}) = \emph{h}, where \emph{x} is the distance that 
#'   maximizes g and \emph{h} is the height of g() at \emph{x} 
#'   computed from the double observer data frame (see below for 
#'   structure of the double observer data frame).
#'   
#'   
#'   \item \bold{Distance of independence specified, height computed from double 
#'   observer system}: Inputs are 
#'   \code{x.scl} = a number greater than or equal to \code{w.lo}
#'   \code{g.x.scl} = a data frame.  In this case, g(\code{x.scl}) = \emph{h}, 
#'   where \emph{h} is computed from the double observer data frame 
#'   (see below for structure of the double observer data frame). 
#'
#' }   
#'   
#'   When \code{x.scl}, \code{g.x.scl}, or \code{observer} are NULL, the routine 
#'   will look for \code{$call.x.scl}, or \code{$call.g.x.scl}, or 
#'   \code{$call.observer} components of the \code{fit} object.  This means the 
#'   3 parameters specified 
#'   during the original call to \code{dfuncEstim} will be used. Later, different 
#'   values can be specified in a direct call to \code{F.gx.estim} 
#'   without having to re-estimate the distance function. Because of this feature, 
#'   the default values of \code{x.scl} = 0 and 
#'   \code{g.x.scl} = 1 and \code{observer} = "both" are specified in the call 
#'   to \code{dfuncEstim}. 
#'   
#' @section Structure of the double observer data frame:  
#' When \code{g.x.scl} is a data frame, it is assumed to contain 
#'   the components \code{$obsby.1} and \code{$obsby.2} (no flexibility on names). 
#'   Each row in the data frame contains data from one sighted target. 
#'   The \code{$obsby.1} and \code{$obsby.2} components are 
#'   TRUE/FALSE (logical) vectors indicating whether 
#'   observer 1 (\code{obsby.1}) or observer 2 (\code{obsby.2}) spotted the target.
#'   
#' @return A list comprised of the following components:
#'   \item{x.scl}{The value of x (distance) at which g() is evaluated.  }
#'   \item{comp2 }{The estimated value of g() when evaluated at \code{x.scl}.  }
#'   
#' @author Trent McDonald
#' 
#' @seealso \code{\link{dfuncEstim}}
#' 
#' @examples \dontrun{
#'   # NOTE, this example is out of date as of version 2.0.x
#'   # Non-double observer example
#'   set.seed(555574)
#'   x <- rnorm(1000) * 100
#'   x <- x[ 0 < x & x < 100 ]
#'   un.dfunc <- dfuncEstim( x, likelihood="uniform", w.hi = 100)    
#'   F.gx.estim(un.dfunc)
#'   gam.dfunc <- dfuncEstim( x, likelihood="Gamma", w.hi = 100)    
#'   F.gx.estim(gam.dfunc)
#'   
#'   # Double observer example
#'   dbl.obs <- data.frame(obsby.1=rbinom(50,1,0.8), obsby.2=rbinom(50,1,0.7))
#'   F.gx.estim(un.dfunc, x.scl=0, g.x.scl=dbl.obs, observer="both" )
#'   # a warning about x.scl < $w.lo is issued.
#'   F.gx.estim(un.dfunc, x.scl="max", g.x.scl=dbl.obs, observer="both" )
#'   F.gx.estim(un.dfunc, x.scl="max", g.x.scl=dbl.obs, observer=1 )
#'   }
#' @keywords model
#' @export

F.gx.estim <- function( fit, x.scl=NULL, g.x.scl=NULL, observer=NULL ){
#
#   Estimate g0 or gx for the distance function in fit.
#

#   Will need measurement units later ----
distUnits <- units(fit$dist)

#   Compute x (the point to evaluate g() at) ----
if( is.null( x.scl ) ){
    x.scl <- fit$call.x.scl
}

#   Compute height of g() at x.scl ----
if( is.null( g.x.scl ) ){
    g.x.scl <- fit$call.g.x.scl
}

#   Double observers? ----
if( is.null( observer ) ){
    observer <- fit$call.observer
}

# overide x.scl for Gamma likelihood
if( !is.character(x.scl) ){
  if( inherits(x.scl, "units") ){ # this if needed cause drop units does not work on plain vector
    isZero <- units::drop_units(x.scl) == 0 
  } else {
    isZero <- x.scl == 0
  }
  if( isZero & fit$like.form == "Gamma" ){
    x.scl <- "max"
    warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
  }
}

if( !is.character(x.scl) ){

    #   x is specified, first make sure w.low < x < w.high, then compute g(x)
    if( !inherits(x.scl, "units") & fit$control$requireUnits ){
      if( x.scl[1] != 0 ){
        stop(paste("Measurement units for x.scl are required.",
                   "Assign units using either:\n", 
                   "units::units(x.scl) <- '<units>' or", 
                   paste0("units::as_units(", x.scl,", <units>) in function call\n"), 
                   "See units::valid_udunits() for valid symbolic units."))
      }
      x.scl <- units::set_units(x.scl, fit$outputUnits, mode = "standard")
    } else if( fit$control$requireUnits ){
      # if we are here, x.scl has units and we require units, convert to the output units
      x.scl <- units::set_units(x.scl, fit$outputUnits, mode = "standard")
    }
  
    if( x.scl < fit$w.lo ){
        x.scl <- fit$w.lo
        warning(paste("x.scl is less than specified lower limit (w.lo). x.scl has been reset to", fit$w.lo))
    } else if( fit$w.hi < x.scl ) {
        x.scl <- fit$x.hi
        warning(paste("x.scl is greater than specified upper limit (w.hi). x.scl has been reset to", fit$w.hi))
    } 
} else if( x.scl == "max" ){
  #   the x that maximizes g() must be estimated

    if( fit$like.form == "Gamma" ){
        r <- fit$par[1]
        lam <- fit$par[2]
        b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
        x.scl <- lam * b * (r - 1)   # this is x that maximizes g() when g is Gamma
    } else if( fit$like.form == "smu"){
        x.scl <- fit$fit$x[which.max(fit$fit$y)]
    } else {
        #   Must compute maximum numerically
        x.scl <- F.maximize.g( fit )
    }
} else {
    x.scl <- NA
    warning("Invalid character string for x.scl specified in F.gx.estim. x.scl set to missing.")
}

#   --------------------------------------------------------------------------------------
#   Now compute g(x)

if( is.data.frame(g.x.scl) ){
    #   Compute g(x) from double observer data
    g.x.scl <- F.double.obs.prob( g.x.scl, observer )
} else {
    #   g(x) is specified, nothing to do except check validity
    if( length(g.x.scl) > 1 ){
        g.x.scl <- g.x.scl[1]
        warning(paste("Vector of g(x) values found in F.gx.estim. Only the first value,", g.x.scl, "has been used."))
    }
    if( g.x.scl < 0 ){
        g.x.scl <- 0
        warning("Impossible g.x.scl < 0 specified in F.gx.estim. g(x) has been reset to 0.")
    } else if( g.x.scl > 1 ){
        g.x.scl <- 1
        warning("Impossible g(x) > 1 specified in F.gx.estim. g(x) reset to 1.")
    } else if( is.character(g.x.scl) ){
        stop(paste0("g.x.scl cannot be character valued. Found '", g.x.scl, "'. Convert this to numeric."))
    }
}
  
list( x.scl = x.scl, g.x.scl = g.x.scl )
}