F.dfunc.estim2 <- function (formula, data, likelihood="halfnorm", point.transects = F, w.lo=0, w.hi=max(dist), 
                            expansions=0, series="cosine", x.scl=0, g.x.scl=1, observer="both", warn=TRUE){
  if (missing(data))
    data <- environment(formula)
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  names(mf)[names(mf)=="formula"] <- "formula"
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  dist <- model.response(mf,"any")
  covars <- if (!is.empty.model(mt)){
    model.matrix(mt, mf, contrasts)
  }
  
  # Find which columns are factors
  factor.names <- NULL
  for(i in 1:ncol(mf)){
    if(class(mf[,i]) == "factor"){
      factor.names <- c(factor.names, names(mf)[i])
    }
  }
  
  ncovars <- ncol(covars)
  if(ncovars==1)
    covars <- NULL
  
  vnames<-dimnames(covars)[[2]]
  
  # dists can be provided as a vector or as a column named 'dist' in a data.frame
  # if d.f, check for a column named 'dist', and extract it
  if(inherits(dist, "data.frame")){
    # Stop and print error if a 'dist' column isn't provided in detection.data
    if(!("dist" %in% names(dist))) stop("There is no column named 'dist' in your dist data.frame.")
    dist <- dist$dist
  }
  
  # Stop and print error if dist vector contains NAs
  if(any(is.na(dist))) stop("Please remove detections for which dist is NA.")
  
  call <- match.call()
  
  strt.lims <- F.start.limits(likelihood, expansions, w.lo, w.hi, dist, covars, point.transects)
  #strt.lims <- NULL
  #for (i in 1:ncovars)
  #  strt.lims[i] <- 1
  fit <- optim(strt.lims$start, F.nLL, lower = strt.lims$lowlimit, upper = strt.lims$uplimit,
               method = c("L-BFGS-B"),
               control = list(trace = 6, maxit = 1000), dist = dist, like = likelihood, covars = covars,
               w.lo = w.lo, w.hi = w.hi, expansions = expansions, series = series, point.transects = point.transects, for.optim = T)
  
  names(fit$par) <- strt.lims$names
  ans <- list(parameters = fit$par, loglik = fit$value, 
              convergence = fit$convergence, like.form = likelihood, 
              w.lo = w.lo, w.hi = w.hi, dist = dist, covars = covars, expansions = expansions, 
              series = series, call = call, call.x.scl = x.scl, call.g.x.scl = g.x.scl, 
              call.observer = observer, fit = fit, factor.names = factor.names, point.transects = point.transects)
  
  ans$loglik <- F.nLL(ans$parameters, ans$dist, covars = ans$covars, like = ans$like.form, w.lo = ans$w.lo, w.hi = ans$w.hi, series = ans$series, expansions = ans$expansions, point.transects = ans$point.transects, for.optim = F)
  
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
  
}  # end function