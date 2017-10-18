#' @name F.maximize.g
#' @aliases F.maximize.g
#' @title Find the coordinate of the maximum of a distance function
#' @description Find the x coordinate that maximizes g(x).
#' @usage F.maximize.g(fit, covars = NULL)
#' @param x An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' @param covars Covariate values to calculate maximum for.
#' @return The value of x that maximizes g(x) in \code{fit}.
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{dfuncEstim}}
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
#' @keywords model
#' @export
#' @importFrom stats optim

F.maximize.g <- function( fit, covars = NULL ){
#
#   Maximize the distance function in fit.  That is, find x such that g(x) is at its
#   maximum.  G is smooth, so this is easy for nlminb.
#

g.neg <-  function(x, params, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0, pointSurvey = F){

    f.like <- match.fun(paste( like, ".like", sep=""))

    if( x < w.lo ){
      x <- w.lo # somehow, optim occasionally passes in -4.1e-15, essentially zero, but 
             # likelihood returns NA and optim bombs.  I'm not happy with optim. Lower
             # argument to optim is clearly 0
    } else if( x > w.hi ){
      x <- w.hi
    }
    
    g.x <- f.like( a = params, dist = x, covars = covars, w.lo=w.lo, w.hi=w.hi, 
                   series = series, expansions = expansions, pointSurvey = pointSurvey )

    #print(c(x=x, params=params,gneg=-g.x*10000000000))
    -g.x * 10000000000
}

x.start <- (fit$w.lo + fit$w.hi) / 10 + fit$w.lo

x.max <- optim(par = x.start, fn = g.neg,  params = fit$parameters, 
               method = "L-BFGS-B", w.lo=fit$w.lo, w.hi=fit$w.hi, like=fit$like.form,
               expansions=fit$expansions, series=fit$series, lower=fit$w.lo, 
               upper=fit$w.hi, covars = covars, pointSurvey = fit$pointSurvey)

if( x.max$convergence != 0 ){
    warning(paste("Maximum of g() could not be found. Message=", x.max$message))
    x.max <- NA
} else {
    x.max <- x.max$par
}

-g.neg(x = x.max, params = fit$parameters, covars = covars, w.lo = fit$w.lo, 
       w.hi = fit$w.hi, like = fit$like.form, expansions = fit$expansions, 
       series = fit$series, pointSurvey = fit$pointSurvey)/10000000000

}
