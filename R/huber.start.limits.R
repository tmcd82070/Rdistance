#' @title Start and limit values for the Huber distance function
#' 
#' @description Computes starting values and limits for the 'huber' 
#' distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
huber.start.limits <- function(ml){

  X <- stats::model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  np <- ncol(X)  
  expan <- ml$expansions

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  wlo <- ml$w.lo
  whi <- ml$w.hi
  w <- dropUnits(whi - wlo)
  
  # Initial values for Theta1, Theta2, and p
  q1  <- dropUnits(stats::quantile(dist, p = 0.33))
  q2  <- dropUnits(stats::quantile(dist, p = 0.67))
  p1 <- 0.05
    
  start <- c(log(q1)
             , rep(zero, np - 1)
             , q2
             , p1
             , rep(zero, expan)
             )
  low   <- c(log(zero)
             , rep(negInf, np - 1 )
             , zero
             , zero
             , rep(negInf, expan)
             )
  high  <- c(log(w * 1.5)
             , rep(posInf, np - 1 )
             , w 
             , 1 - fuzz
             , rep(posInf, expan)
             )
  nms <- c(colnames(X), "theta2", "p")
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}