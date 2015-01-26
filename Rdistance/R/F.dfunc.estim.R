F.dfunc.estim2=function (dist, likelihood = "halfnorm", w.lo = 0, w.hi = max(dist,na.rm=T), 
          expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1, 
          observer = "both", warn = TRUE) 
{
  
  #### I changed the default w.hi so that it does not return NA if dist contains NA
  
  dist2=dist #Store with NAs
  dist=dist[!is.na(dist)] #Remove NAs for maximizing likelihood
  
  
  call <- match.call()
  strt.lims <- F.start.limits(likelihood, expansions, w.lo, 
                              w.hi, dist)
  fit <- nlminb(strt.lims$start, F.nLL, lower = strt.lims$lowlimit, 
                upper = strt.lims$uplimit, control = list(trace = 0, 
                                                          iter.max = 1000), dist = dist, like = likelihood, 
                w.lo = w.lo, w.hi = w.hi, expansions = expansions, series = series)
  names(fit$par) <- strt.lims$names
  ans <- list(parameters = fit$par, loglik = fit$objective, 
              convergence = fit$convergence, like.form = likelihood, 
              w.lo = w.lo, w.hi = w.hi, dist = dist2, expansions = expansions, 
              series = series, call = call, call.x.scl = x.scl, call.g.x.scl = g.x.scl, 
              call.observer = observer, fit = fit)
  class(ans) <- "dfunc"
  gx <- F.gx.estim(ans)
  ans$x.scl <- gx$x.scl
  ans$g.x.scl <- gx$g.x.scl
  fuzz <- 1e-06
  low.bound <- any(fit$par <= (strt.lims$lowlimit + fuzz))
  high.bound <- any(fit$par >= (strt.lims$uplimit - fuzz))
  if (fit$convergence != 0) {
    if (warn) 
      warning(fit$message)
  }
  else if (low.bound | high.bound) {
    ans$convergence <- -1
    ans$fit$message <- "One or more parameters at its boundary."
    if (warn) 
      warning(ans$fit$message)
  }
  ans
}