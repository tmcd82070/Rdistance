F.dfunc.estim<-function(dist, likelihood="halfnorm", w.lo=0, w.hi=max(dist), expansions=0, series="cosine")
{
# Remember to describe naming convention in help files.  Naming convention is <form>.nLL() returns the negative log liklihood value (i.e., a scalar). 
# <form>.like() returns the likelihood evaluated at every point (i.e., a vector)
# likelihood is a string naming <form>


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

#   Check for convergence and parameters at the boundaries
fuzz <- 1e-6
low.bound <- any( fit$par <= (strt.lims$lowlimit + fuzz) )
high.bound <- any( fit$par >= (strt.lims$uplimit - fuzz) )

gx <- list( x.scl=0, g.x.scl=1 )
if( fit$convergence != 0 ){
    warning(fit$message)
} else if( low.bound | high.bound ){
    fit$convergence <- -1
    fit$message <- "One or more parameters at its boundary."
    warning(fit$message)
} else {
#   Set g(0), or g(x).
    gx <- F.gx.estim( fit, likelihood, w.lo )  # returns components for x and g(x)
}


ans <- list(    parameters=fit$par,
                loglik = fit$objective,
                convergence = fit$convergence, 
                like.form = likelihood,
                x.scl = gx$x.scl,
                g.x.scl = gx$g.x.scl,
                w.lo = w.lo,
                w.hi = w.hi,
                dist = dist,
                expansions=expansions,
                series = series,
                call = call,                
                fit=fit )               
    
class( ans ) <- "dfunc" 

    
ans
}
  
