mlEstimates <- function( modelList
                       , strt.lims
                       ){

  # there are no missing distances because stats::model.frame 
  # dropped them CHECK THIS!!!
  
  optimFunc <- getOption("Rdistance_optimizer")
  
  # pass in dist vector, even though it's in modelList
  # because don't want to evaluate model.response every  time
  # likelihood is called.
  
  dist <- stats::model.response(modelList$mf)
  X <- stats::model.matrix(modelList$mf)
  
  if (optimFunc == "optim"){
      fit <- optim(
          strt.lims$start
          , F.nLL
          , lower = units::drop_units(strt.lims$lowlimit) # safe
          , upper = units::drop_units(strt.lims$uplimit)
          , hessian = TRUE
          , control = list(trace = 0,
              maxit = control$maxIters,
              factr = control$likeTol,
              pgtol = control$likeTol)
          , method = c("L-BFGS-B")
          , dist = dist
          , ml = modelList
          , for.optim = T)
  } else if (optimFunc == "nlminb"){
      fit <- nlminb(
            start = strt.lims$start
          , objective = F.nLL
          , lower = strt.lims$lowlimit
          , upper = strt.lims$uplimit
          , control = list(trace = 0
              , eval.max = getOption("evalMax")
              , iter.max = getOption("maxIters")
              , rel.tol = getOption("likeTol")
              , x.tol = getOption("coefTol")
              )
          , dist = dist
          , ml = modelList
          , for.optim = T
          )
      names(fit)[names(fit) == "evaluations"]<-"counts"
  
      fit$hessian <- secondDeriv(fit$par,
          F.nLL,
          eps = getOption("hessEps"),
          dist = dist,
          ml = modelList,
          for.optim = T
          )
  } else {
      stop(paste("Unknown optimizer function. Found", optimFunc))
  }

  if (fit$fit$convergence != 0) {
    if (warn) warning("fit did not converge, or converged to (Inf,-Inf)")
    varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
  } else if (!any(is.na(fit$hessian)) & !any(is.infinite(fit$hessian))){
      qrh <- qr(fit$hessian)
      if (qrh$rank < nrow(fit$hessian)) {
        if (warn) warning("Singular variance-covariance matrix.")
        varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
      } else {
        varcovar <- tryCatch(solve(fit$hessian), error = function(e){NaN})
        if (length(varcovar) == 1 && is.nan(varcovar)){
          if (warn) warning("Singular variance-covariance matrix.")
          varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
        }
      }
  }
  
  names(fit$par) <- strt.lims$names
 
  fit 

}