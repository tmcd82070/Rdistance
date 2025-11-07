#' @title Numeric Integration of One-step Function
#' 
#' @description
#' Compute integral of the one-step distance function
#' using numeric integration. This function is only called for 
#' oneStep functions that contain expansion factors. 
#' 
#' @inheritParams integrateOneStepPoints
#' @inheritParams dE.single
#' @inheritParams integrateNumeric
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' The \code{\link{oneStep.like}} function has an extremely large 
#' discontinuity at Theta. Accurate numeric integration requires 
#' inserting Theta and Theta+ (a value just larger than Theta) 
#' into the series of points being evaluated. Because this creates 
#' un-equal intervals, the Trapazoid rule must be used. 
#' Rdistance's Simpson's rule routine 
#' (\code{\link{integrateNumeric}}) will not work for oneStep likelihoods 
#' that have expansions. 
#' 
#' @seealso \code{\link{integrateNumeric}}; 
#' \code{\link{integrateOneStepLines}}; \code{\link{integrateOneStepPoints}}
#' 
#' @export
#' 
integrateOneStepNumeric <- function(object
                            , newdata = NULL
                            , w.lo = NULL
                            , w.hi = NULL
                            , Units = NULL
                            , expansions = NULL
                            , series = NULL
                            , isPoints = NULL
                              ){
  
  # we know we are in the oneStep with expansions > 0 case here
  likelihood = "oneStep"
  f.like <- utils::getFromNamespace(paste0( likelihood, ".like"), "Rdistance")    
  
  # need this if b/c sometimes this is called from nLL (object is just a 
  # matrix of parameters) and other times it is called from EDR (object is 
  # fitted object)
  if( inherits(object, "dfunc") ){
    w.lo <- object$w.lo
    w.hi <- object$w.hi # override input if it's given
    Units <- object$outputUnits # override if given
    expansions <- object$expansions
    series <- object$series
    isPoints <- is.points(object)
    object <- stats::predict(object = object
                        , newdata = newdata
                        , type = "parameters"
    )
    object[,1] <- log(object[,1]) # predict returns real param, f.like needs link value
    # If object has expansions, their coefficients come back from predict
  } 
  
  Theta <- setUnits(object[,1], Units) # link scale
  p <- object[1,2] # p is always constant, this saves some space
  if( expansions > 0 ){
    coefLocs <- (ncol(object)-(expansions-1)):(ncol(object))
    expCoefs <- object[1,coefLocs] # constant across observations
  }
  
  nInts <- getOption("Rdistance_intEvalPts") # odd not a requirement here
  
  # This really slows down oneStep with expansions, but, no other way
  # If there are covariates, Theta is potentially different on every row
  XIntOnly <- matrix(1, nrow = 2*nInts, ncol = 1) 
  outArea <- rep(NA, length(Theta))
  zero <- setUnits(0, Units)
  
  uniqueTheta <- unique(Theta)
  thetaFuzz <- setUnits(getOption("Rdistance_fuzz"), Units)

  for(i in 1:length(uniqueTheta)){
    theta <- uniqueTheta[i]
    thetaNoUnits <- dropUnits( theta) 
    expTheta <- exp( thetaNoUnits ) 
    expTheta <- setUnits(expTheta, Units) # with units
    posTheta <- Theta == theta # use later
    
    # Key: insert theta and theta+ into distances, use unequal intervals
    d = c(seq(zero, expTheta, length=nInts) 
             , seq(expTheta + thetaFuzz, w.hi - w.lo, length=nInts ))
    dx <- diff(d)  

    y <- f.like(
      a = c(thetaNoUnits, p)
      , dist = d
      , covars = XIntOnly
      , w.hi = w.hi
    )
    y <- y$L.unscaled # (nInts x 1) = (length(d) X 1) in this case

    # yL <- y #debugging
    
    if( expansions > 0 ){
      W <- expTheta - w.lo
      exp.terms <- Rdistance::expansionTerms(a = c(0  # theta and p not used in expTerms
                                                   , 0
                                                   , expCoefs)
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
    
    thetaArea <- sum( dx * (y[-1,] + y[-nrow(y), ]) / 2 ) 
    outArea[posTheta] <- thetaArea
  }
  
  # plot(d, y[,1], main = "in intOneStepNumeric", type="p")
  # 
  # lines(d, yL[,1], col = "red")
  # abline(v = expTheta)
  
  outArea # units should be object$outputUnits^2
  
}
