#' @name F.gx.estim
#' @aliases F.gx.estim
#' @title F.gx.estim - Estimate g(0) or g(x).
#' @description Estimate g(0) or g(x) for a specified distance function.
#' @usage F.gx.estim(fit, x.scl=NULL, g.x.scl=NULL, observer=NULL)
#' @param fit An estimated \code{dfunc} object.  See \code{F.dfunc.estim}.
#' @param x.scl The x coordinate (a distance) at which to scale the distance funciton to \code{g.x.scl}.  See Details.
#' @param g.x.scl Height of the distance function at coordinate x. i.e., the distance function 
#'   will be scaled so that g(\code{x.scl}) = \code{g.x.scl}. See Details.
#' @param observer A numeric scalar or text string specifying whether observer 1 or observer 2 or both were full-time observers. 
#'   This parameter dictates which set of observations form the denominator 
#'   of a double observer system.   
#'   If, for example, observer 2 was a data recorder and part-time observer, or if observer 2 
#'   was the pilot, set \code{observer} = 1.  If \code{observer} = 1, observations by observer 1 not seen 
#'   by observer 2 are ignored. The estimate of detecton in this case is the ratio of number of targets seen by both observers 
#'   to the number seen by both plus the number seen by just observer 2. If observer = "both", the 
#'   computation goes both directions.
#' @details There are several estimation cases covered by the inputs \code{x.scl} and \code{g.x.scl}:
#'   
#'   (1) g(0) = 1  (the default): Inputs are \code{x.scl} = 0, \code{g.x.scl} = 1. 
#'   Note that \code{x.scl} will be set to \code{w.lo}, which is not necessarily 0.
#'   
#'   (2) User specified g(\code{x.scl}) = \code{g.x.scl}:  Inputs are \code{x.scl} = a number greater than or equal 
#'   to \code{w.lo}, \code{g.x.scl} = 
#'     a number between 0 and 1.
#'   
#'   (3) Maximum g() specified: Inputs are \code{x.scl}="max", \code{g.x.scl} =  a number between 0 and 1.  In this case, 
#'   g() is scaled such that g(x.max) = \code{g.x.scl}, where x.max is the distance that maximizes g.  
#'   x.max is computed and returned.
#'   
#'   
#'   (4) Maximum g() estimated by double observer system: Inputs are \code{x.scl}="max", \code{g.x.scl} =  a data frame. 
#'   In this case, g(x.max) = h, where x.max is the distance that maximizes g and h is the height of g() at x.max. 
#'   h is computed from the double observer data frame (see below for structure of the double observer data frame).
#'   
#'   
#'   (5) Distance of independence specified, height computed from double observer system: Inputs are 
#'   \code{x.scl} = a number greater than or equal to \code{w.lo}
#'   \code{g.x.scl} = a data frame.  In this case, g(\code{x.scl}) = h, where h is computed from the double observer data frame 
#'   (see below for structure of the double observer data frame). 
#'   
#'   
#'   When \code{x.scl}, \code{g.x.scl}, or \code{observer} are NULL, the routine will look for \code{$call.x.scl}, or \code{$call.g.x.scl}, or 
#'   \code{$call.observer} components of the \code{fit} object.  This means the 3 parameters to be specified 
#'   during the original call to \code{F.dfunc.estim}. Later, different values can be specified in a call to \code{F.gx.estim} 
#'   without having to re-estimate the distance function. Because of this feature, the default values of \code{x.scl} = 0 and 
#'   \code{g.x.scl} = 1 and \code{observer} = "both" are specified in the call to \code{F.dfunc.estim}. 
#'   
#'   Structure of the double observer data frame:  When \code{g.x.scl} is a data frame, it is assumed to contain 
#'   the components \code{$obsby.1} and \code{$obsby.2} (no flexibility on names). 
#'   These components are TRUE/FALSE (logical) vectors indicating whether 
#'   observer 1 (\code{obsby.1}) or observer 2 (\code{obsby.2}) spotted the target.
#' @return A list comprised of the following components:
#'   \item{x.scl}{The value of x (distance) at which g() is evaluated.  }
#'   \item{comp2 }{The estimated value of g() when evaluated at \code{x.scl}.  }
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{F.dfunc.estim}}
#' @examples \dontrun{
#'   # Non-double observer example
#'   set.seed(555574)
#'   x <- rnorm(1000) * 100
#'   x <- x[ 0 < x & x < 100 ]
#'   un.dfunc <- F.dfunc.estim( x, likelihood="uniform", w.hi = 100)    
#'   F.gx.estim(un.dfunc)
#'   gam.dfunc <- F.dfunc.estim( x, likelihood="Gamma", w.hi = 100)    
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

F.gx.estim <- function( fit, x.scl=NULL, g.x.scl=NULL, observer=NULL ){
#
#   Estimate g0 or gx for the distance function in fit.
#

#   --------------------------------------------------------------------------------------
#   First, compute x (the point to evaluate g() at)
if( is.null( x.scl ) ){
    x.scl <- fit$call.x.scl
}
if( is.null( g.x.scl ) ){
    g.x.scl <- fit$call.g.x.scl
}
if( is.null( observer ) ){
    observer <- fit$call.observer
}

if( !is.character(x.scl) ){
    if( x.scl == 0 & fit$like.form == "Gamma" ){
        x.scl <- "max"
        warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
    }
}

if( !is.character(x.scl) ){

    #   x is specified, first make sure w.low < x < w.high, then compute g(x)
    if( x.scl < fit$w.lo ){
        x.scl <- fit$w.lo
        warning(paste("x less than lower limit specified. Reset x.scl to lower limit (i.e.,", fit$w.lo, ")"))
    } else if( fit$w.hi < x.scl ) {
        x.scl <- fit$x.hi
        warning(paste("x greater than upper limit specified. Reset x.scl to upper limit (i.e.,", fit$w.hi, ")"))
    } 
} else if( x.scl == "max" ){
    #   the x that maximizes g() must be estimated

    if( fit$like.form == "Gamma" ){
        r <- fit$par[1]
        lam <- fit$par[2]
        b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
        x.scl <- lam * b * (r - 1)   # this is x that maximizes g() when g is Gamma
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
    #   g(x) is specified, nothing to do except check range
    if( g.x.scl < 0 ){
        g.x.scl <- 0
        warning("Impossible g(x) < 0 specified in F.gx.estim. g(x) reset to 0.")
    } else if( g.x.scl > 1 ){
        g.x.scl <- 1
        warning("Impossible g(x) > 1 specified in F.gx.estim. g(x) reset to 1.")
    }
}
  
list( x.scl = x.scl, g.x.scl = g.x.scl )
}