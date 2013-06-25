F.dfunc.estim<-function(dist, likelihood="halfnorm", w.lo=0, w.hi=max(dist), expansions=0, series="cosine", 
     x.scl=0, g.x.scl=1, observer="both", warn=TRUE)
{
# Remember to describe naming convention in help files.  Naming convention is <form>.nLL() returns the negative log liklihood value (i.e., a scalar).
# <form>.like() returns the likelihood evaluated at every point (i.e., a vector)
# likelihood is a string naming <form>
#   x.scl, g.x.scl, and observer = parameters passed to F.g.estim. See F.gx.estim.


#Expansion terms work - jg - 5/4/10

call <- match.call()

#   Set starting values and limits for parameters
strt.lims <- F.start.limits( likelihood, expansions, w.lo, w.hi, dist )


#   Minimize the negative sum of the loglikelihood.
fit<- nlminb(strt.lims$start, F.nLL,  lower=strt.lims$lowlimit,
      upper=strt.lims$uplimit, control=list(trace=0,iter.max=1000),
      dist=dist, like=likelihood, w.lo=w.lo, w.hi=w.hi, expansions=expansions, series=series)

#   Assign names to parameters here
names(fit$par) <- strt.lims$names

#   Make dfunc object
ans <- list(    parameters=fit$par,
                loglik = fit$objective,
                convergence = fit$convergence,
                like.form = likelihood,
                w.lo = w.lo,
                w.hi = w.hi,
                dist = dist,
                expansions=expansions,
                series = series,
                call = call,
                call.x.scl = x.scl,
                call.g.x.scl = g.x.scl,
                call.observer = observer,
                fit=fit
                 )

class( ans ) <- "dfunc"

#   Set g(0) or g(x)
gx <- F.gx.estim( ans )  # returns components for x and g(x)
ans$x.scl <- gx$x.scl
ans$g.x.scl <- gx$g.x.scl

#   At this point, call.x.scl and call.g.x.scl contain the orginial variables input to this routine.
#   It is necessary to store these so that inputs like "max" and a data frame will persist. 
#   x.scl and g.x.scl are the actual values of x and g(x). 



#   Check for convergence and parameters at the boundaries
fuzz <- 1e-6
low.bound <- any( fit$par <= (strt.lims$lowlimit + fuzz) )
high.bound <- any( fit$par >= (strt.lims$uplimit - fuzz) )

if( fit$convergence != 0 ){
    if(warn) warning(fit$message)
} else if( low.bound | high.bound ){
    ans$convergence <- -1
    ans$fit$message <- "One or more parameters at its boundary."
    if(warn) warning(ans$fit$message)
}

ans
}
