#' @title Set starting values and limits for parameters of Rdistance functions
#' 
#' @description Return reasonable starting values and limits (boundaries) for the parameters of 
#'   distance functions.  Starting values and limits are specified for 
#'   all likelihoods and expansion terms.  This function is called by 
#'   other routines in \code{Rdistance}, and is not intended to 
#'   be called by the user.
#'   
#' @param like String specifying the likelihood for the distance function.  Possible values are 
#'   "hazrate" for hazard rate likelihood, "halfnorm" for the half 
#'   normal likelihood, "uniform" for the uniform likelihood, 
#'   "negexp" for the negative exponential likelihood, and 
#'   "Gamma" for the gamma likelihood.
#' @param expan Number of expansion terms to include. Valid values are 0, 1, ..., 3.
#' @param w.lo Lower or left-truncation limit of the distances.  Normally, 0.
#' @param w.hi Upper or right-truncation limit of the distances. This is the maximum off-transect distance that could be observed.
#' @param dist The vector of observed off-transect distances being analyzed.  This vector is only required for \code{like} = "Gamma" and "halfnorm".
#' @param covars Matrix of covariate values.
#' @param pointSurvey Boolean. TRUE if point transect data, FALSE if line transect data.
#' @details The number of parameters to be fitted is 
#'   \code{expan + 1 + 1*(like \%in\% c("hazrate", "uniform"))}.
#'   This is the length of all vectors returned in the output list.
#' @return A list containing the following components
#'   \item{start}{Vector of reasonable starting values for parameters of the likelihood and expansion terms. }
#'   \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
#'   \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
#'   \item{names}{Vector of names for the likelihood parameters and expansion terms.}
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#' @seealso \code{\link{dfuncEstim}}
#' @examples 
#'   dist <- sparrowDetectionData$dist
#'   units(dist) <- "m"
#'   wl <- units::as_units(0, "m")
#'   wh <- units::as_units(1000, "m")
#'   
#'   F.start.limits("uniform", 0, wl, wh, dist)
#'   F.start.limits("uniform", 1, wl, wh, dist)
#'   F.start.limits("uniform", 2, wl, wh, dist)
#'   F.start.limits("uniform", 3, wl, wh, dist)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist)
#'   F.start.limits("halfnorm", 1, wl, wh, dist)
#'   F.start.limits("halfnorm", 2, wl, wh, dist)
#'   F.start.limits("halfnorm", 3, wl, wh, dist)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 1, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 2, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 3, wl, wh, dist, pointSurvey = TRUE)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 1, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 2, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 3, wl, wh, dist, data.frame(A=1, B=2))
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 1, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 2, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 3, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   
#'   F.start.limits("hazrate", 0, wl, wh, dist)
#'   F.start.limits("hazrate", 1, wl, wh, dist)
#'   F.start.limits("hazrate", 2, wl, wh, dist)
#'   F.start.limits("hazrate", 3, wl, wh, dist)
#'   
#'   F.start.limits("negexp", 0, wl, wh, dist)
#'   F.start.limits("negexp", 1, wl, wh, dist)
#'   F.start.limits("negexp", 2, wl, wh, dist)
#'   F.start.limits("negexp", 3, wl, wh, dist)
#'   
#'   F.start.limits("Gamma", 0, wl, wh, dist)
#' @keywords models
#' @export
F.start.limits <- function( like, expan, w.lo, w.hi, dist, covars = NULL, pointSurvey = FALSE ){
  #
  #   Establish starting value for parameters, and limits passed to the optimizer
  #
  
  #   Number of parameters
  if(!is.null(covars)){
  ncovars <- ncol(covars)
  }else{ncovars <- 1}
  
  np <- expan + 1*(like %in% c("hazrate","uniform")) + ncovars
  
  w <- w.hi - w.lo
  
  distUnits <- units(dist)
  
  zero <- units::as_units(0, distUnits)
  zeroLogUnits <- log(units::as_units(1, distUnits))
  negInf <- units::as_units(-Inf, distUnits)
  posInf <- -negInf
  negInfLog <- log(zero)
  posInfLog <- log(posInf)
  
  #   No starting values given
  if( like == "hazrate" ){
    if( ncovars > 1 ){
      start <- c(log(.5*w), 
                 rep(log(units::as_units(exp(0),distUnits)), ncovars-1), 
                 log(units::as_units(exp(1),distUnits)),
                 rep(log(units::as_units(exp(0),distUnits)), expan))
      low   <- c(log(units::as_units(exp(-10),distUnits)), 
                 rep(negInfLog, ncovars-1), 
                 log(units::as_units(exp(0.01),distUnits)), 
                 rep(negInfLog, expan))
      high  <- c(posInfLog, 
                 rep( posInfLog, ncovars-1), 
                 posInfLog, 
                 rep( posInfLog, expan))
    } else {
      start <- c(.5*w, 
                 units::as_units(1,distUnits),
                 rep(units::as_units(0,distUnits), np - 2))
      low   <- c(units::as_units(0,distUnits), 
                 rep(negInf, ncovars-1), 
                 units::as_units(.01, distUnits), 
                 rep(negInf, expan))
      high  <- c(posInf, 
                 rep( posInf, ncovars-1), 
                 posInf, 
                 rep( posInf, expan))
    }
    if( ncovars > 1 ){
      nms <- c(colnames(covars), "Beta")
    } else {
      nms <- c("Sigma", "Beta")
    }
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:expan, sep=""))
    }
    
  } else if( like == "halfnorm" ){
    if( ncovars > 1 ){
      start <- c(log(sqrt(sum( (dist - w.lo)^2 )/length(dist))), 
                 rep(zeroLogUnits, np - 1))
      low <- c(rep(negInfLog, np))
      high  <- c(posInfLog, rep( posInfLog, np - 1 ))
    } else if(pointSurvey){
      start <- c(sqrt(sum( (dist - w.lo)^2 )/length(dist)), rep(zero, np - 1))
      low <- c(zero, rep(negInf, np - 1 ))
      high  <- c(posInf, rep( posInf, np - 1 ))
    } else{
      start <- c(sqrt(sum( (dist - w.lo)^2 )/length(dist)), 
                 rep(zero, np - 1))
      low <- c(zero, rep(negInf, np - 1 ))
      high  <- c(posInf, rep( posInf, np - 1 ))
    }
    if( ncovars > 1 ){
      nms <- colnames(covars)
    } else {
      nms <- c("Sigma")
    }
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:(np-ncovars), sep=""))
    }
    
  } else if( like == "uniform" ){
    if( ncovars > 1 ){
      start <- c(log(.1*w), 
                 rep(zeroLogUnits, ncovars-1), 
                 log(units::as_units(exp(1), distUnits)), 
                 rep(zeroLogUnits, expan))
      low   <- c(log(units::as_units(exp(-14), distUnits)), 
                 rep(negInfLog, ncovars-1), 
                 zeroLogUnits, 
                 rep(negInfLog, expan))
      high  <- c(log(w), rep(posInfLog, ncovars-1), posInfLog, rep( posInfLog, expan))
    } else {
      start <- c(.1*w, 
                 units::as_units(1, distUnits), 
                 rep(zero, np - 2))
      low   <- c(units::as_units(1e-6, distUnits), 
                 rep(negInf, ncovars-1), 
                 zero, 
                 rep(negInf, expan))
      high  <- c(w, 
                 rep(posInf, ncovars-1), 
                 posInf, 
                 rep( posInf, expan))
    }
    if( ncovars > 1 ){
      nms <- c(colnames(covars), "Knee")
    } else {
      nms <- c("Threshold", "Knee")
    }
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:expan, sep=""))
    }
    
  } else if( like == "negexp" ){
    if( ncovars > 1 ){
      start <- c(zero, rep(zero, np - 1))
      low   <- c(negInf, rep(negInf, np - 1 ))
      high  <- c(posInf, rep( posInf, np - 1 ))
    } else {
      start <- c(units::as_units(1,distUnits), 
                 rep(zero, np - 1))
      low   <- c(zero, 
                 rep(negInf, np - 1 ))
      high  <- c(posInf, rep( posInf, np - 1 ))
    }
    if( ncovars > 1 ){
      nms <- colnames(covars)
    } else {
      nms <- c("Beta")
    }
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:expan, sep=""))
    }
    
    
  } else if( like == "Gamma" ){
    d <- dist[ w.lo <= dist & dist <= w.hi ]
    d <- d[ d > zero ] # even though 0 is legit, can't take log of it
    s <- units::drop_units( log( mean(d, na.rm=TRUE) ) - mean( log(d), na.rm=TRUE ) )
    s2 <- (s-3)^2 + 24*s
    if( s2 < 0 ) s2 <- 0
    r <- (3 - s + sqrt( s2 )) / (12*s)
    if( r <= 1 ) r <- 1.01
    b <- ( (r-1) / exp(1) )^(r-1) / gamma(r)
    lam <- mean(d,na.rm=TRUE) / (r * b)
    r <- units::as_units(r, distUnits)
    
    if( ncovars > 1 ){
      start <- c(log(r), rep(0, ncovars-1), log(lam))
      low   <- c(log(units::as_units(1.0001, distUnits)), rep(negInfLog, ncovars-1), zeroLogUnits)
      nms <- colnames(covars)
    } else {
      start <- c(r, lam)
      low   <- c(units::as_units(1.0001,distUnits), zero)
      nms <- c("Shape", "Scale")
    }
    high  <- c(posInf, posInf)
    
  } else {
    #   Assume this is a user-defined likelihood
    fn <- match.fun( paste(like, ".start.limits", sep="") )
    ans <- fn(dist, expan, w.lo, w.hi)
    start <- ans$start
    low <- ans$lowlimit
    high <- ans$highlimit
    nms <- ans$names
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}
