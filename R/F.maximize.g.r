F.maximize.g <- function( fit, covars ){
#
#   Maximize the distance function in fit.  That is, find x such that g(x) is at its
#   maximum.  G is smooth, so this is easy for nlminb.
#

g.neg <-  function(x, params, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0, point.transects = F){

    f.like <- match.fun(paste( like, ".like", sep=""))

    g.x <- f.like( a = params, dist = x, covars = covars, w.lo=w.lo, w.hi=w.hi, series = series, expansions = expansions, point.transects = point.transects )

    -g.x * 10000000000
}

x.start <- (fit$w.lo + fit$w.hi) / 10 + fit$w.lo

x.max <- optim(par = x.start, fn = g.neg,  params = fit$parameters, method = "L-BFGS-B", w.lo=fit$w.lo, w.hi=fit$w.hi, like=fit$like.form,
    expansions=fit$expansions, series=fit$series, lower=fit$w.lo, upper=fit$w.hi, covars = covars, point.transects = fit$point.transects)

if( x.max$convergence != 0 ){
    warning(paste("Maximum of g() could not be found. Message=", x.max$message))
    x.max <- NA
} else {
    x.max <- x.max$par
}

-g.neg(x = x.max, params = fit$parameters, covars = covars, w.lo = fit$w.lo, w.hi = fit$w.hi, like = fit$like.form,
       expansions = fit$expansions, series = fit$series, point.transects = fit$point.transects)/10000000000

}
