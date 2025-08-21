#' @title Start and limit values for hazrate distance function
#' 
#' @description Compute starting values and limits for the hazard rate distance
#' function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
hazrate.start.limits <- function (ml){
  
  X <- stats::model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
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
  
  # Only time dist will not have units is when user overrides requirement
  # Nonetheless, need to remove units b/c likelihood is unitless
  dMin <- units::set_units(dMin, NULL)
  dMax <- units::set_units(dMax, NULL)
  w <- units::set_units(w, NULL)
  medDist <- units::set_units(medDist, NULL)
  
  start <- c(log(0.8 * medDist)   # Sigma 
             , rep(zero, ncovars-1)    # any covars
             , 1               # k
             , rep(zero, expan))        # any expansions
  low   <- c(negInf
             , rep(negInf, ncovars-1)
             , 0.5
             , rep(negInf, expan))
  high  <- c(posInf
             , rep( posInf, ncovars-1)
             , 20 
             , rep( posInf, expan))
  nms <- c(colnames(X), "k")
    
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }

  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}
