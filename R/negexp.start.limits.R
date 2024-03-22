#' @title negexp.start.limits - Start and limit values for negexp distance function
#' 
#' @description Compute starting values and limits for the negative 
#' exponential distance
#' function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
negexp.start.limits <- function (ml){
  
  X <- stats::model.matrix(ml$mt, ml$mf)
  dist <- stats::model.response(ml$mf)  
  
  ncovars <- ncol(X)

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  expan <- ml$expansions
  dMin <- max( min(dist), ml$w.lo )
  dMax <- min( max(dist), ml$w.hi )
  w <- ml$w.hi - ml$w.lo
  medDist <- stats::median(dist)
  if(is.null(medDist) || is.na(medDist) || is.infinite(medDist)){
    medDist <- ml$w.lo + w / 2
  }
  if( inherits(dist, "units") ){
    # Only time dist will not have units is when user overrides requirement
    # otherwise this always runs
    dMin <- units::drop_units(dMin)
    dMax <- units::drop_units(dMax)
    w <- units::drop_units(w)
    medDist <- units::drop_units(medDist)
  }
  
  logLoWid <- (log(medDist) - log(dMin))
  logLoWid <- max(logLoWid, fuzz)
  logLoWid <- 1.0 / logLoWid
  
  logHiWid <- (log(dMax) - log(medDist))
  logHiWid <- max(logHiWid, fuzz)
  logHiWid <- 1.0 / logHiWid
  
  startIntercept <-  logLoWid + logHiWid
  startIntercept <- max(startIntercept, 1)
  
  start <- c(log(startIntercept)
             , rep(zero, ncovars - 1)
             , rep( posInf, expan)
             )
  low   <- c(negInf
             , rep(negInf, ncovars - 1 )
             , rep( posInf, expan)
             )
  high  <- c(posInf*w
             , rep( posInf*w, ncovars - 1 )
             , rep( posInf, expan)
             )
  nms <- colnames(X)
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}