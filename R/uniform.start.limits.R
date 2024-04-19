#' @title uniform.start.limits - Start and limit values for uniform distance function
#' 
#' @description Compute starting value and limits for the uniform
#' distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
uniform.start.limits <- function (ml){

  X <- model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  ncovars <- nCovars(X)  
  expan <- ml$expansions

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  

  # Should not have to convert units.  wlo and whi should 
  # be in 'outputUnits'. Safe to drop units.
  w <- units::set_units(ml$w.hi - ml$w.lo, NULL)
  m <- units::set_units(mean(dist, na.rm = TRUE), NULL)
  maxD <- units::set_units(max(dist, na.rm = TRUE), NULL)
  minD <- units::set_units(min(dist, na.rm = TRUE), NULL)
  if(is.na(m) || is.infinite(m) || is.nan(m) || (m <= 0)){
    strt <- log(w * 0.5)
  } else {
    strt <- log(m)
    strt <- log(w * .9)
  }
  if(is.na(minD) || is.infinite(minD) || is.nan(minD) || (minD <= 0)){
    lowA <- log(getOption("Rdistance_zero"))
  } else {
    lowA <- log(minD)
  }
  
  start <- c(strt
             , rep(zero, ncovars - 1)
             , rep(zero, expan)
             )
  if( ncovars <= 1 ){
    # (Intercept)-only model. Use tighter bounds.
    low <- lowA
    high <- log(maxD)
  } else {
    # We have covariates
    low <- rep(negInf, ncovars)
    high <- rep(posInf, ncovars)
  }
  # Add bounds for expansions
  low <- c(low
           , rep(negInf, expan)
  )
  high  <- c(high
             , rep(posInf, expan)
  )

  nms <- colnames(X)
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:ml$expansions, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}