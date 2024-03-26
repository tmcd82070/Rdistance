#' @title huber.start.limits - Start and limit values for huber distance function
#' 
#' @description Compute starting value and limits for the huber 
#' distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
huber.start.limits <- function(ml){

  X <- stats::model.matrix(ml$mt, ml$mf)
  dist <- stats::model.response(ml$mf)  
  
  np <- ncol(X)  
  expan <- ml$expansions

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  # list(start=max(dist)*.75,
  #      lowlimit=ml$w.lo,
  #      highlimit=ml$w.hi,
  #      names="Max")
 
  wlo <- ml$w.lo
  whi <- ml$w.hi
  w <- whi - wlo
  
  # Should not have to convert units.  wlo and whi should 
  # be in 'outputUnits'. Safe to drop units.
  # wlo <- units::set_units(wlo, units(w), mode = "standard")
  # whi <- units::set_units(whi, units(w), mode = "standard")
  wlo <- units::drop_units(wlo)
  whi <- units::drop_units(whi)
  w <- units::drop_units(w)
  
  start <- c(log(w*.5)
             , rep(zero, np - 1)
             , log(w)
             , rep(zero, expan)
             )
  low   <- c(log(zero)
             , rep(negInf, np - 1 )
             , log(zero)
             , rep(negInf, expan)
             )
  high  <- c(log(w * 1.5)
             , rep(posInf, np - 1 )
             , log(w * 2)
             , rep(posInf, expan)
             )
  nms <- colnames(X)
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:ml$expansions, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}