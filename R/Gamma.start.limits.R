#' @title Gamma.start.limits - Start and limit values for Gamma parameters.
#' 
#' @description Compute starting values and limits for the Gamma likelihood
#' function. 
#' 
#' @inheritParams logistic.like
#' 
#' @inherit logistic.start.limits return
#' 
#' @export
Gamma.start.limits <- function (dist
                                , covars
                                , expansions
                                , w.lo
                                , w.hi
                                ){
  
  d <- dist[ w.lo <= dist & dist <= w.hi ]
  d <- d[ d > units::set_units(0, "m") ] # even though 0 is fine, can't take log of it
  s <- units::drop_units( log( mean(d, na.rm=TRUE) ) - mean( log(d), na.rm=TRUE ) )
  s2 <- (s-3)^2 + 24*s
  if( s2 < 0 ){ 
    s2 <- 0 
  }
  r <- (3 - s + sqrt( s2 )) / (12*s)
  if( r <= 1 ) {
    r <- 1.01
  }
  b <- ( (r-1) / exp(1) )^(r-1) / gamma(r)
  lam <- mean(d,na.rm=TRUE) / (r * b)
  if( inherits(lam, "units")){
    # only time this does not fire is when user overides units
    lam <- units::drop_units(lam)
  }

  negInf <- -.Machine$double.xmax
  posInf <- -negInf
  zero <- .Machine$double.xmin
  
  expanStart <- rep(0, expansions)
  expanLow <- rep(negInf, expansions)
  expanHigh <- rep(posInf, expansions)
  
  ncovars <- ncol(covars)
  if( !is.null(covars) ){
    start <- c(log(lam), rep(0, ncovars-1), r, expanStart)
    low   <- c(zero, rep(negInf, ncovars-1), 1 + .Machine$double.eps, expanLow)
    nms <- c(colnames(covars), "Shape")
    high  <- c(posInf, rep(posInf, ncovars-1), posInf, expanHigh)
  } else {
    start <- c(lam, r, expanStart)
    low   <- c(zero, 1 + .Machine$double.eps, expanLow)
    nms <- c("Scale", "Shape")
    high  <- c(posInf, posInf, expanHigh)
  }
  
  if(expansions > 0){
    nms <- c(nms, paste( "a", 1:expansions, sep=""))
  }
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}