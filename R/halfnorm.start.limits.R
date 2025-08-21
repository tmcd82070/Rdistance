#' @title Start and limit values for halfnorm distance function
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
  
  X <- stats::model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  expan <- ml$expansions
  ncovars <- nCovars(X)

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  dist2 <- dist - ml$w.lo
  # Only time dist2 will not have units is when user overides requirement
  # otherwise this always runs
  dist2 <- units::set_units(dist2, NULL)
  dist2 <- dist2[dist2 > 0]
  sdHalf <- max(sqrt(sum( dist2^2 )/length(dist2)), 10*fuzz)
  
  start <- c(log(sdHalf)
             , rep(zero, ncovars - 1)
             , rep(zero, expan)
             )
  if( ncovars <= 1 ){
    # (Intercept)-only model. Use tighter bounds.
    low <- log(zero)
    high <- log(posInf)
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
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}
