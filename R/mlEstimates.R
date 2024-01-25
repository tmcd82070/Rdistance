mlEstimates <- function( modelList
                       , strt.lims
                       ){

    distNoNA <- ml$dist[ !is.na(ml$dist) ]
    
    if (ml$control$optimizer == "optim"){
        fit <- optim(strt.lims$start,
            F.nLL,
            lower = units::drop_units(strt.lims$lowlimit), # safe
            upper = units::drop_units(strt.lims$uplimit),
            hessian = TRUE,
            control = list(trace = 0,
                maxit = control$maxIters,
                factr = control$likeTol,
                pgtol = control$likeTol),
            method = c("L-BFGS-B"),
            dist = distNoNA,
            like = ml$likelihood,
            covars = ml$covars,
            w.lo = ml$w.lo,
            w.hi = ml$w.hi,
            expansions = ml$expansions,
            series = ml$series,
            pointSurvey = ml$pointSurvey,
            for.optim = T)
    } else if (control$optimizer == "nlminb"){
        fit <- nlminb(start = strt.lims$start
            , objective = F.nLL
            , lower = strt.lims$lowlimit
            , upper = strt.lims$uplimit
            , control = list(trace = 0
                , eval.max = ml$control$evalMax
                , iter.max = ml$control$maxIters
                , rel.tol = ml$control$likeTol
                , x.tol = ml$control$coefTol
                )
            , dist = distNoNA
            , like = ml$likelihood
            , covars = ml$covars
            , w.lo = ml$w.lo
            , w.hi = ml$w.hi
            , expansions = ml$expansions
            , series = ml$series
            , pointSurvey = ml$pointSurvey
            , for.optim = T
            )
        names(fit)[names(fit) == "evaluations"]<-"counts"
    
        fit$hessian <- secondDeriv(fit$par,
            F.nLL,
            eps = ml$control$hessEps,
            dist = distNoNA,
            like = ml$likelihood,
            covars = ml$covars,
            w.lo = ml$w.lo,
            w.hi = ml$w.hi,
            expansions = ml$expansions,
            series = ml$series,
            pointSurvey = ml$pointSurvey,
            for.optim = T
            )
    } else {
        stop("Unknown optimizer function in control object")
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
    

}