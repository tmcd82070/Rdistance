#' @title Gamma.start.limits - Start and limit values for Gamma distance function
#' 
#' @description Compute starting values and limits for the Gamma distance
#' function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
Gamma.start.limits <- function (ml){
  
  X <- model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  ncovars <- ncol(X)

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  d <- dist[ dist > units::set_units(0, "m") ] # even though 0 is fine, can't take log of it
  s <- units::drop_units( log( mean(d, na.rm=TRUE) ) - mean( log(d), na.rm=TRUE ) ) # there are no NA's
  s2 <- (s-3)^2 + 24*s
  if( s2 < zero ){ 
    s2 <- zero 
  }
  r <- (3 - s + sqrt( s2 )) / (12*s)
  if( r <= 1.0 ) {
    r <- 1.01
  }
  b <- ( (r-1) / exp(1) )^(r-1) / gamma(r)
  lam <- mean(d,na.rm=TRUE) / (r * b)
  if( inherits(lam, "units")){
    # only time this does not fire is when user overides units
    lam <- units::drop_units(lam)
  }

  expanStart <- rep(0, ml$expansions)
  expanLow <- rep(negInf, ml$expansions)
  expanHigh <- rep(posInf, ml$expansions)
  
  start <- c(log(lam), rep(0, ncovars-1), r, expanStart)
  low   <- c(zero, rep(negInf, ncovars-1), 1 + fuzz, expanLow)
  high  <- c(posInf, rep(posInf, ncovars-1), posInf, expanHigh)

  nms <- c(colnames(X), "Shape")
  if(expansions > 0){
    nms <- c(nms, paste( "a", 1:expansions, sep=""))
  }

  names(start) <- nms
  names(low) <- nms
  names(high) <- nms  
  
  list( start=start, low=low, high=high, names=nms )
  
}