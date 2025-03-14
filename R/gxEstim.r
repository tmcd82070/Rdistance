#' @title gxEstim - Estimate g(0) or g(x)
#' 
#' @description Estimate distance function scaling factor
#' , g(0) or g(x), for a specified distance function.
#' 
#' @param fit An estimated \code{dfunc} object.  See \code{dfuncEstim}.
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
#'   is unknown. 
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
#'   will look for and use \code{$call.x.scl}, or \code{$call.g.x.scl}, or 
#'   \code{$call.observer} components of the \code{fit} object for whichever 
#'   of these three parameters is missing.  Later, different 
#'   values can be specified in a direct call to \code{F.gx.estim} 
#'   without having to re-estimate the distance function. Because of this feature, 
#'   the default values in \code{dfuncEstim} are \code{x.scl} = 0 and 
#'   \code{g.x.scl} = 1 and \code{observer} = "both". 
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
#' 
#' @seealso \code{\link{dfuncEstim}}
#' 
#' @examples 
#' 
#' data(sparrowDf)
#' fit <- dfuncEstim(sparrowDf, dist ~ groupsize(groupsize))
#' gxEstim(fit)
#'   
#' fit <- dfuncEstim(sparrowDf, dist ~ groupsize(groupsize)
#'                 , x.scl = units::set_units(50,"m")
#'                 , g.x.scl = 0.75)
#' gxEstim(fit)
#' plot(fit)
#' abline(h=0.75)
#' abline(v=units::set_units(50,"m"))
#'   
#' @keywords model
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
  
  # overide x.scl for Gamma likelihood
  if( !is.character(x.scl) ){
    x.scl.xUnits <- units::set_units(x.scl, NULL)
    isZero <- x.scl.xUnits == 0
    if( isZero & fit$likelihood == "Gamma" ){
      x.scl <- "max"
      # warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
    }
  }
  
  if( !is.character(x.scl) ){
    # x is specified, first make sure w.low < x < w.high, then compute g(x)
    if( x.scl < fit$w.lo ){
          x.scl <- fit$w.lo
          warning(paste("x.scl is less than specified lower limit (w.lo). x.scl has been reset to", fit$w.lo))
      } else if( fit$w.hi < x.scl ) {
          x.scl <- fit$x.hi
          warning(paste("x.scl is greater than specified upper limit (w.hi). x.scl has been reset to", fit$w.hi))
      } 
  } else if( x.scl == "max" ){
    #   the x that maximizes g() must be estimated
    if( fit$likelihood == "Gamma" & (fit$expansions == 0) ){
      r <- fit$par[1]
      lam <- fit$par[2]
      b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
      x.scl <- lam * b * (r - 1)   # the Mode = the x that maximizes g() when g is Gamma
    } else if( fit$likelihood == "smu"){
      x.scl <- fit$fit$x[which.max(fit$fit$y)]
    } else if( fit$likelihood %in% c("logistic", "halfnorm", "negexp", "hazrate", "triangle", "huber") & 
               fit$expansions == 0 ){
      # All these are monotonically negative
      x.scl <- fit$w.lo 
    } else {
      # if we are here, we have an unknown likelihood.
      # WHAT ABOUT GAMMA WITH EXPANSIONS?
      # user-defined likelihood, and we don't know 
      # where it's maximum is. Hence, we compute the maximum numerically. 
      # It is on the user to make sure their likelihood is well behaved and 
      # gives a nice maximum.
      # x.scl <- maximize.g( fit )
      stop("unknown likelihood")  # should never happen
    }
    # x.scl came in as "max", we must return one with units attached. 
    # maximize.g returns a unitless x.scl
    x.scl <- units::set_units(x.scl, fit$outputUnits, mode = "standard")
    
  } else {
    x.scl <- units::set_units(NA, fit$outputUnits, mode = "standard")
    warning("Invalid character string for x.scl specified in gxEstim. x.scl set to missing.")
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