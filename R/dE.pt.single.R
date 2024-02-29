#' @title dE.pt.single - Estimate single-observer point-transect distance function
#' 
#' @description Fits a detection function to radial off-point
#' distances collected by a single observer. 
#' 
#' @inheritParams dE.lt.single
#' 
#' @inherit dE.lt.single return
#' 
#' @inherit dE.lt.single seealso
#' 
#' @examples 
#' thrasherDf <- RdistDf( thrasherSiteData, thrasherDetectionData )
#' 
#' dfunc <- dfuncEstim(formula = dist ~ 1
#'                   , detectionData = thrasherDf)
#' dfunc
#' plot(dfunc)                   
#'
#' @keywords model
#' @export
dE.pt.single <- function(   data
                            , formula
                            , likelihood = "halfnorm"
                            , w.lo = units::set_units(0,"m")
                            , w.hi = NULL
                            , expansions = 0
                            , series = "cosine"
                            , x.scl = units::set_units(0,"m")
                            , g.x.scl = 1
                            , warn = TRUE
                            , outputUnits = NULL
 ){

  if ( likelihood == "uniform" ){
    .Deprecated(new = "logistic.like"
                , package = "Rdistance"
                , msg = paste("'unform.like' is depricated. Use 'logistic'.\n"
                              , "Switching to 'logistic' likelihood.")
                , old = "uniform.like")
    likelihood <- "logistic"
  }

  # all parameters go into parseModel because they need to become
  # components for the output list, not just formula.
  modelList <- parseModel(formula = formula
                        , likelihood = likelihood
                        , w.lo = w.lo
                        , w.hi = w.hi
                        , expansions = expansions
                        , series = series
                        , x.scl = x.scl
                        , g.x.scl = g.x.scl
                        )

  strt.lims <- F.start.limits(modelList)

  # Perform optimization
  fit <- mlEstimates( modelList )
  

  ans <- list(parameters = fit$par,
    varcovar = varcovar,
    loglik = fit$value,
    convergence = fit$convergence,
    like.form = likelihood,
    w.lo = w.lo,
    w.hi = w.hi,
    detections = data.frame(dist, groupSize),
    covars = covars,
    model.frame = mf,
    siteID.cols = siteID.cols,
    expansions = expansions,
    series = series,
    call = cl,
    call.x.scl = x.scl,
    call.g.x.scl = g.x.scl,
    call.observer = observer,
    fit = fit,
    factor.names = factor.names,
    pointSurvey = pointSurvey,
    formula = formula,
    control = control,
    outputUnits = outUnits)

ans$loglik <- F.nLL(ans$parameters
, ans$detections$dist
, covars = ans$covars
, like = ans$like.form
, w.lo = ans$w.lo
, w.hi = ans$w.hi
, series = ans$series
, expansions = ans$expansions
, pointSurvey = ans$pointSurvey
, for.optim = F)

# Assemble results
class(ans) <- "dfunc"
if ( ans$like.form != "Gamma" ){
# not absolutely necessary. Could estimate these later in print and plot methods.
# but this saves a little time.
gx <- F.gx.estim(ans)
ans$x.scl <- gx$x.scl
ans$g.x.scl <- gx$g.x.scl
} else {
# Special case of Gamma
ans$x.scl <- x.scl
ans$g.x.scl <- g.x.scl
}

# ---- Check parameter boundaries ----
fuzz <- 1e-06
low.bound <- fit$par <= (strt.lims$lowlimit + fuzz)
high.bound <- fit$par >= (strt.lims$uplimit - fuzz)
if (fit$convergence != 0) {
if (warn) warning(fit$message)
}
if (any(low.bound)) {
ans$convergence <- -1
messL <- paste(paste(strt.lims$names[low.bound], "parameter at lower boundary.")
, collapse = "; ")
ans$fit$message <- messL
if (warn) warning(ans$fit$message)
} else {
messL <- NULL
}
if (any(high.bound)) {
ans$convergence <- -1
messH <- paste(paste(strt.lims$names[high.bound], "parameter at upper boundary.")
, collapse = "; ")
ans$fit$message <- c(messL, messH)
if (warn) warning(ans$fit$message)
}

ans

} # end function
