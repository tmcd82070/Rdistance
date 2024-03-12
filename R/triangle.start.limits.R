#' @title triangle.start.limits - Start and limit values for triangle distance function
#' 
#' @description Compute starting values and limits for the triangle 
#' distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
triangle.start.limits <- function (ml){

  X <- stats::model.matrix(ml$mt, ml$mf)
  dist <- stats::model.response(ml$mf)  
  
  np <- ncol(X)  

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  # list(start=max(dist)*.75,
  #      lowlimit=ml$w.lo,
  #      highlimit=ml$w.hi,
  #      names="Max")
 
  start <- c(log(max(dist)*.75)
             , rep(zero, np - 1))
  low   <- c(negInf
             , rep(negInf, np - 1 ))
  high  <- c(posInf
             , rep(posInf, np - 1 ))
  nms <- colnames(X)
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:ml$expansions, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}