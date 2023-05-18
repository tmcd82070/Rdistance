#' @title Find the coordinate of the maximum of a distance function
#' 
#' @description Find the x coordinate that maximizes g(x).
#' 
#' @param fit An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' 
#' @param covars Covariate values to calculate maximum for.
#' 
#' @return The value of x that maximizes g(x) in \code{fit}.
#' 
#' @seealso \code{\link{dfuncEstim}}
#' 
#' @examples \dontrun{
#' # Fake data
#' set.seed(22223333)
#' x <- rgamma(100, 10, 1)
#' 
#' fit <- dfuncEstim( x, likelihood="Gamma", x.scl="max" )
#' 
#' F.maximize.g( fit )  # should be near 10.
#' fit$x.scl            # same thing
#' }
#' 
#' @keywords model
#' @export
#' @importFrom stats optim 

F.maximize.g <- function( fit, covars = NULL ){

g.neg <-  function(x, 
                   params, 
                   covars = NULL, 
                   like, 
                   w.lo, 
                   w.hi, 
                   series, 
                   expansions=0, 
                   pointSurvey = F,
                   correctUnits){
    f.like <- match.fun(paste( like, ".like", sep=""))

    if( x < w.lo ){
      x <- w.lo # somehow, optim occasionally passes in -4.1e-15, essentially zero, but 
             # likelihood returns NA and optim bombs.  I'm not happy with optim. Lower
             # argument to optim is clearly 0
    } else if( x > w.hi ){
      x <- w.hi
    }
    
    # above here, we needed x, w.lo, and w.hi to be unitless
    # because they had to pass through optim(). Now, we need 
    # them to have units because we are about to call the likelihood.
    x <- units::set_units(x, correctUnits, mode = "standard")
    w.lo <- units::set_units(w.lo, correctUnits, mode = "standard")
    w.hi <- units::set_units(w.hi, correctUnits, mode = "standard")
    
    g.x <- f.like( a = params, 
                   dist = x, 
                   covars = covars, 
                   w.lo=w.lo, 
                   w.hi=w.hi, 
                   series = series, 
                   expansions = expansions, 
                   pointSurvey = pointSurvey )

    -g.x * 10000000000
    
}

# Strategy for handling units:  make sure everything is 
# converted to the same units, then drop units because optim
# does not propagate them.  w.lo and w.hi are PROBABLY already 
# in the correct units, but make sure.

correctUnits <- fit$outputUnits

wlo <- units::set_units(fit$w.lo, correctUnits, mode = "standard")
whi <- units::set_units(fit$w.hi, correctUnits, mode = "standard")

wlo <- units::drop_units(wlo)
whi <- units::drop_units(whi)

x.start <- (wlo + whi) / 10 + wlo

x.max <- optim(par = x.start, 
               fn = g.neg,  
               method = "L-BFGS-B", 
               lower=wlo, 
               upper=whi, 
               
               params = fit$parameters, 
               w.lo=wlo,
               w.hi=whi,
               like=fit$like.form,
               expansions=fit$expansions, 
               series=fit$series, 
               covars = covars, 
               pointSurvey = fit$pointSurvey,
               correctUnits = correctUnits)


if( x.max$convergence != 0 ){
    warning(paste("Maximum of g() could not be found. Message=", x.max$message))
    x.max <- NA
} else {
    x.max <- x.max$par
}

x.max
}
