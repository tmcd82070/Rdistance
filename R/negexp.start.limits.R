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
#' @importFrom stats median
negexp.start.limits <- function (ml){
  
  X <- model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  ncovars <- nCovars(X)

  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  expan <- ml$expansions

  # there should not be any distances outside (w.lo,w.hi)
  d <- dist - ml$w.lo
  medDist <- stats::median(d)
  medDist <- units::set_units(medDist, NULL)
  
  if(is.null(medDist) || 
     is.na(medDist) || 
     is.infinite(medDist) || 
     (medDist <= zero)){
    w <- ml$w.hi - ml$w.lo
    medDist <- ml$w.lo + w / 2
    medDist <- units::set_units(medDist, NULL)
  } 
  
  startIntercept <- -log(medDist) # = log(1/medDist)
  
  # 1/medDist is MOM estimate of lambda (lambda = slope of neg exp)
  # log(1/medDist) is MOM estimate on link scale

  start <- c(startIntercept
             , rep(zero, ncovars - 1)
             , rep(zero, expan)
             )
  low   <- c(negInf
             , rep(negInf, ncovars - 1 )
             , rep(negInf, expan)
             )
  high  <- c(posInf
             , rep( posInf, ncovars - 1 )
             , rep( posInf, expan)
             )
  nms <- colnames(X)
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}