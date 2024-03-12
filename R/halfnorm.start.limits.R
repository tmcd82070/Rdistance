#' @title halfnorm.start.limits - Start and limit values for halfnorm distance function
#' 
#' @description Compute starting values and limits for the half normal distance
#' function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
halfnorm.start.limits <- function (ml){
  
  X <- stats::model.matrix(ml$mt, ml$mf)
  dist <- stats::model.response(ml$mf)  
  
  expan <- ml$expansions
  ncovars <- ncol(X)

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")

  dist2 <- dist - ml$w.lo
  if( inherits(dist2, "units") ){
    # Only time dist2 will not have units is when user overides requirement
    # otherwise this always runs
    dist2 <- units::drop_units(dist2)
  }
  dist2 <- dist2[dist2 > 0]
  sdHalf <- max(sqrt(sum( dist2^2 )/length(dist2)), 10*fuzz)
  
  start <- c(log(sdHalf)
             , rep(zero, ncovars - 1)
             , rep( posInf, expan)
             )
  low <- c(
    rep(log(zero), ncovars)
    , rep( posInf, expan)
  )
  high  <- c(
    rep( log(posInf), ncovars )
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