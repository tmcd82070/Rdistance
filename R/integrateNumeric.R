#' @title Numeric Integration
#' 
#' @description
#' Numerically integrate under a distance function. 
#' 
#' @inheritParams integrateOneStepPoints
#' @inheritParams dE.single
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @param isPoints Boolean. TRUE if integration is for point surveys.  
#' FALSE for line-transect surveys. Line-transect surveys integrate 
#' under the distance function, g(x), while point surveys integrate under 
#' the distance function times distances, xg(x). 
#' 
#' @section Numeric Integration: 
#' Rdistance uses Simpson's composite 1/3 rule to numerically 
#' integrate distance functions from 0 to the maximum sighting distance
#' (\code{w.hi - w.lo}). The number of points evaluated 
#' during numerical integration is controlled by 
#' \code{options(Rdistance_intEvalPts)} (default 101).
#' Option 'Rdistance_intEvalPts' must be odd because Simpson's rule
#' requires an even number of intervals. 
#' Lower values of 'Rdistance_intEvalPts' increase calculation speeds; 
#' but, decrease accuracy.
#' 'Rdistance_intEvalPts' must be >= 5.  A warning is thrown if 
#' 'Rdistance_intEvalPts' < 29. Empirical tests by the author 
#' suggest 'Rdistance_intEvalPts' values >= 30 are accurate 
#' to several decimal points for smooth distance functions
#' (e.g., hazrate, halfnorm, negexp)
#' and that all 'Rdistance_intEvalPts' >= 101 produce 
#' identical results if the distance function is not smooth. 
#'   
#' \emph{Details}: Let \code{n} = \code{options(Rdistance_intEvalPts)}.
#' Evaluate the distance function at \code{n} equal-spaced 
#' locations \{f(x0), f(x1), ..., f(xn)\} between 0 and (w.hi - w.lo). 
#' Simpson's composite approximation to the area under the curve is
#' \deqn{\frac{1}{3}h(f(x_0) + 4f(x_1) + 2f(x_2) + 
#'      4f(x_3) + 2f(x_4) + ... + 2f(x_{n-2}) + 
#'      4f(x_{n-1}) + f(x_{n}))}{(1/3)h(f(x0) + 4f(x1) + 2f(x2) + 
#'      4f(x3) + 2f(x4) + ... + 2f(x(n-2)) + 4f(x(n-1)) + f(xn))}
#' where \eqn{h} is the interval size (w.hi - w.lo) / n.
#' 
#' Physical units on the return values
#' are the original (linear) units if \code{object} contains line-transect data
#' (e.g., [m]), or square of the original units if \code{object} contains
#' point-transect data (e.g., [m^2]). Point-transect units are squared because
#' the likelihood is the product of the detection function (which is unitless) 
#' and distances (which have units).  
#' 
#' 
#' @examples
#' 
#' # A halfnorm distance function 
#' fit <- dfuncEstim(sparrowDf, dist~1, likelihood = "halfnorm")
#' 
#' exact <- integrateHalfnormLines(fit)[1,] # exact area
#' apprx <- integrateNumeric(fit)[1]  # Numeric approx
#' pd <- options(digits = 20)
#' cbind(exact, apprx)
#' absDiff <- abs(apprx - exact) 
#' absDiff
#' options(pd)
#' 
#' # halfnorm approx good to this number of digits
#' round(log10(absDiff),1)  
#'
#' @export
#' 
integrateNumeric <- function(object
                           , newdata = NULL
                           , w.lo = NULL
                           , w.hi = NULL
                           , Units = NULL
                           , expansions = NULL
                           , series = NULL
                           , isPoints = NULL
                           , likelihood = NULL
                             ){
  
  if( inherits(object, "dfunc") ){
    w.lo <- object$w.lo
    w.hi <- object$w.hi 
    Units <- object$outputUnits 
    expansions <- object$expansions
    series <- object$series
    isPoints <- is.points(object)
    likelihood <- object$likelihood 
    
    # Now convert object to parameters
    object <- stats::predict(object = object
                             , newdata = newdata
                             , type = "parameters"
    )
    object[,1] <- log(object[,1]) # predict returns real param, f.like needs link value
    # If object has expansions, their coefficients come back from predict
  } 

  # This check of nInts and intCoefs slows things down, but integrateNumeric 
  # gets called from ESW/EDR and NLL. User could change nInts or intCoeffs 
  # in between calls to ESW/EDR and NLL, so we have not choice but to check.
  nInts <- getOption("Rdistance_intEvalPts") # already checked it's odd, in parseModel::checkNevalPts
  intCoefs <- getOption("Rdistance_intCoefs") # Next, check intCoefs are correct
  if( nInts != length(intCoefs) || sum(intCoefs) != (3*(nInts-1)) ){
    intCoefs <- simpsonCoefs( nInts ) # oddity checked
    options("Rdistance_intCoefs" = intCoefs )
  }
  zero <- units::set_units(0, Units, mode="standard")

  d <- seq(zero, w.hi - w.lo, length=nInts) 
  dx <- d[2] - d[1]  # or (w.hi - w.lo) / (nInts-1); could do diff(dx) if unequal intervals

  # don't need covars since params are always computed
  XIntOnly <- matrix(1, nrow = length(d), ncol = 1) 

  f.like <- utils::getFromNamespace(paste0( likelihood, ".like"), "Rdistance")    

  y <- f.like(
      a = object
    , dist = d
    , covars = XIntOnly
    , w.hi = w.hi - w.lo # I don't think we need w.hi in f.like here
  )
  y <- y$L.unscaled # (nInts x n) = (length(d) X nrow(parms))

  if( expansions > 0 ){
    # we know that likelihood is a differentiableLikelihoods (not oneStep)
    W <- rep(w.hi - w.lo, nrow(object))
    exp.terms <- Rdistance::expansionTerms(a = object
                                           , d = d
                                           , series = series
                                           , nexp = expansions
                                           , w = W)
    y <- y * exp.terms
    y[ !is.na(y) & (y <= 0) ] <- getOption("Rdistance_zero")

  }

  if(isPoints){
    y <- d * y  # element-wise
  }
  
  outArea <- intCoefs * y  # (n vector) * (n X k)
  outArea <- colSums(outArea) * dx / 3

  outArea 
  
}