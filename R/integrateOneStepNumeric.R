#' @title Numeric Integration of One-step Function
#' 
#' @description
#' Compute integral of the one-step distance function
#' using numeric integration. This function is only called for 
#' oneStep functions that contain expansion factors. 
#' 
#' @inheritParams integrateOneStepPoints 
#' @inheritParams effectiveDistance 
#' 
#' @details 
#' The \code{\link{oneStep.like}} function has an extremely large 
#' discontinuity in Theta. Accurate numeric integration requires special 
#' use of the Trapazoid rule. Rdistance's Simpson's rule routine 
#' (\code{\link{integrateNumeric}}) will not work for oneStep. 
#' 
#' @inherit integrateOneStepPoints return
#' 
#' @seealso \code{\link{integrateNumeric}}; 
#' \code{\link{integrateOneStepLines}}; \code{\link{integrateOneStepPoints}}
#' 
#' @examples
#' 
#' fit <- dfuncEstim(thrasherDf, dist~1, likelihood = "oneStep")
#' integrateOneStepPoints(fit, newdata = data.frame(`(Intercept)`=1))
#' EDR(fit, newdata = data.frame(`(Intercept)`=1))
#' 
#' # Check: 
#' Theta <- exp(fit$par[1])
#' Theta <- units::set_units(Theta, "m")
#' p <- fit$par[2]
#' w.hi <- fit$w.hi
#' w.lo <- fit$w.lo
#' g.at0 <- w.lo
#' g.atT <- Theta
#' g.atTPlusFuzz <- (((1-p) * Theta) / ((w.hi - Theta) * p))*Theta
#' g.atWhi <- (((1-p) * Theta) / ((w.hi - Theta) * p))*w.hi
#' area.0.to.T <- (Theta - w.lo) * (g.atT - g.at0) / 2 # triangle; Theta^2/2
#' area.T.to.w <- (w.hi - Theta) * (g.atTPlusFuzz + g.atWhi) / 2 # trapazoid
#' area <- area.0.to.T + area.T.to.w
#' edr <- sqrt( 2*area )
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
  
  Theta <- units::set_units(object[,1], Units, mode="standard") # link scale
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
  zero <- units::set_units(0, Units, mode = "standard")
  
  uniqueTheta <- unique(Theta)
  thetaFuzz <- units::set_units(getOption("Rdistance_fuzz")
                              , Units
                              , mode = "standard")

  for(i in 1:length(uniqueTheta)){
    theta <- uniqueTheta[i]
    thetaNoUnits <- units::set_units( theta, NULL ) 
    expTheta <- exp( thetaNoUnits ) 
    expTheta <- units::set_units(expTheta, Units, mode="standard") # with units
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

    yL <- y #debugging
    
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