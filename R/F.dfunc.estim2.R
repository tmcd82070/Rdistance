#' @name F.dfunc.estim2
#' @aliases F.dfunc.estim2
#' @title Estimate a detection function from distance-sampling data.
#' @description Fit a specific detection function to a set of observed off-transect distances.
#' @usage F.dfunc.estim2(formula, data, likelihood="halfnorm", point.transects = F, w.lo=0, w.hi=max(dist), 
#'   expansions=0, series="cosine", x.scl=0, g.x.scl=1, observer="both", warn=TRUE), likelihood = "halfnorm", point.transects = F, w.lo)
#' @param formula Formula object containing names of vectors containing distance values and any covariates on detection. (eg dist ~ 1, dist ~ covar1 + covar2)
#' @param data Data frame containing columns with names in \code{formula}.
#' @param likelihood String specifying the likelihood to assume. Valid values at present are "uniform", "halfnorm", "hazrate", "negexp", and "Gamma".
#' @param point.transects Boolean. TRUE if using point transect data, FALSE if using line transect data.
#' @param w.lo Lower or left-truncation limit of the distances in distance data.  This is the minimum possible off-transect distance. Default is 0.
#' @param w.hi Upper or right-truncation limit of the distances in \code{dist}. This is the maximum off-transect distance that could be observed. Default is the maximum of \code{dist}.
#' @param expansions A scalar specifying the number of terms in \code{series} to compute. Depending on the series, this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.
#' @param series If \code{expansions} > 0, this string specifies the type of expansion to use. Valid values at present are 'simple', 'hermite', and 'cosine'. 
#' @param x.scl This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
#' @param g.x.scl This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
#' @param observer This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
#' @param warn A logical scaler specifying whether to issue an R warning if the estimation did not converge or if one or more parameter estimates are at their boundaries.  
#'   For estimation, \code{warn} should generally be left at its default value of \code{TRUE}.  When computing bootstrap confidence intervals, setting \code{warn = FALSE} 
#'   turns off annoying warnings when an iteration does not converge.  Regardless of \code{warn}, messages about convergence and boundary conditions are printed by \code{print.dfunc},
#'   \code{print.abund}, and \code{plot.dfunc}, so there should be little harm in setting \code{warn = FALSE}.
#' @details Given a specified likelihood (e.g., "halfnorm"), maximum likelihood is used to estimate the parameter(s) of that likelihood function that best fit the distance data.
#'   When the resulting detection function is plotted (see Examples), bins are plotted behind the detection function for visualization; however, the function is fit to the actual data, not to the bins.
#' @value  An object of class 'dfunc'.  Objects of class 'dfunc' are lists 
#'   containing the following components:
#'   \item{parameters}{The vector of estimated parameter values. 
#'     Length of this vector for built-in likelihood is the 
#'     number of expansion terms plus 1 plus 1 if the likelihood is 
#'     either 'hazrate' or 'uniform'. }
#'   \item{loglik}{The maximized value of the log likelihood (more specifically, the minimized value
#'                                                            of the negative log likelihood).}
#'   \item{convergence}{The convergence code. This code is returned by 
#'   \code{nlminb}.  Values other than 0 indicate suspect 
#'     convergence.}
#'   \item{like.form}{The form of the likelihood assumed. This is 
#'     the value of the argument \code{likelihood}. }
#'   \item{w.lo}{Left-truncation value used during the fit.}
#'   \item{w.hi}{Right-truncation value used during the fit.}
#'   \item{dist}{The input vector of observed off-transect distances.}
#'   \item{expansions}{The number of expansion terms used during estimation.}
#'   \item{series}{The type of expansion used during estimation.}
#'   \item{call}{The original call of this function.}
#'   \item{fit}{The fitted object returned by \code{nlminb}.  See documentation 
#'     for \code{nlminb}.}
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}
#'         Aidan McDonald, WEST Inc., \email{aidan@mcdcentral.org}
#' @seealso See \code{\link{uniform.like}} for details on the "uniform", "halfnorm", "hazrate", and "negexp" likelihoods.  See \code{\link{Gamma.like}} for details on "Gamma". 
#'   And see package vignettes for information on custom, user-defined likelihoods.  See also \code{\link{F.abund.estim}}, \code{\link{F.automated.CDA}}
#' @examples # Load the example dataset of sparrow detections from package
#'   data(sparrow.detections)
#'   
#'   # Fit multiple detection functions to perpendicular, off-transect distances
#'   un.dfunc <- F.dfunc.estim(dist ~ 1, sparrow, likelihood="uniform", w.hi = 150)
#'   hn.dfunc <- F.dfunc.estim(dist ~ 1, thrasher, likelihood="halfnorm", w.hi = 150, point.transects = T)
#'   ne.dfunc <- F.dfunc.estim(dist ~ shrub + obsever, sparrow, likelihood="negexp", w.hi = 150, expansions = 2, series = "simple")
#'   hz.dfunc <- F.dfunc.estim(dist ~ observer + bare, thrasher, likelihood="hazrate", w.hi = 150, point.transects = T, expansions = 5, series = "cosine)
#'   ga.dfunc <- F.dfunc.estim(dist ~ 1, sparrow, likelihood="Gamma", w.hi = 150, x.scl="max") 
#'   
#'   # Plot the first four detection functions
#'   par(mfrow=c(2,2))
#'   plot(un.dfunc)
#'   plot(hn.dfunc)
#'   plot(ne.dfunc)
#'   plot(hz.dfunc)
#' @keyword model

F.dfunc.estim2 <- function (formula, data = NULL, likelihood="halfnorm", point.transects = F, w.lo=0, w.hi=max(dist), 
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
               control = list(trace = 0, maxit = 1000), dist = dist, like = likelihood, covars = covars,
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