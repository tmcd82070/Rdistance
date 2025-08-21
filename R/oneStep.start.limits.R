#' @title oneStep likelihood start and limit values
#' 
#' @description Compute starting values and limits 
#' for the oneStep distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @seealso \code{\link{oneStep.like}}
#' 
#' @examples
#' # make 'model list' object
#' # Boundary is 10, p is 100 / 120 = 0.833
#' library(Rdistance)
#' whi <- 50
#' x <- c( runif(100, min=0, max=10), runif(20, min=10, max=whi))
#' x <- units::set_units(x, "m")
#' detectDf <- data.frame(transect = 1, dist = x)
#' siteDf <- data.frame(transect = 1, length = units::set_units(10,"m"))
#' distDf <- RdistDf(siteDf, detectDf)
#' ml <- parseModel(distDf
#'             , formula = dist ~ 1
#'             , w.lo = 0
#'             , w.hi = units::set_units(whi, "m")
#'             )
#'             
#'
#' sl <- oneStep.start.limits(ml)
#' hist(x, n = 20)
#' abline(v = exp(sl$start["(Intercept)"]))
#' 
#' 
#' @export
oneStep.start.limits <- function (ml){
  
  X <- model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  expan <- ml$expansions
  ncovars <- nCovars(X)
  w.lo <- ml$w.lo
  w.hi <- ml$w.hi

  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  # w.lo and w.hi should always have values, so the following 
  # never fires, but just in case.
  if(is.null(w.lo) | is.na(w.lo)){
    w.lo <- min(dist, na.rm = T)
  }
  if(is.null(w.hi) | is.na(w.hi)){
    w.hi <- max(dist, na.rm = T)
  }

  # Only time dist2 will not have units is when user overides requirement
  # Regardless, convert w.lo and w.hi, then drop units
  w.lo <- units::set_units(w.lo, units(dist), mode = "standard")
  w.hi <- units::set_units(w.hi, units(dist), mode = "standard")
  x    <- units::set_units(dist, NULL)
  w.lo <- units::set_units(w.lo, NULL)
  w.hi <- units::set_units(w.hi, NULL)
  
  # scale so 0 <= xx <= 1
  x <- x - w.lo
  x <- x[x >= 0]
  
  x <- x / (w.hi - w.lo) 
  x <- x[x <= 1]
  
  x <- x[!is.na(x)]

  # Remember: 0 <= xx < 1
  # But: values == 0 or 1 are not useful. Remove them.
  x <- x[ fuzz <= x & x <= (1 - fuzz) ]
  n <- length(x)
  
  if( n > 1 ){
    x <- sort( x ) # order statistics

    # MLE estimator ----
    r <- 1:(n-1)
    R1 <- r * (log(r) - log(x[r])) + 
      (n - r) * (log(n - r) - log(1 - x[r])) 
    rPlus1 <- 2:n
    R2 <- r * (log(r) - log(x[rPlus1])) + 
      (n - r) * (log(n - r) - log(1 - x[rPlus1])) 
    R <- pmax(R1, R2)
    p.ind <- which.max(R)
    theta.MLE <- x[p.ind]
    p.MLE <- sum(x <= theta.MLE) / n
    
    theta.MLE <- theta.MLE * (w.hi - w.lo) + w.lo
    
  } else {
    # case with no data
    theta.MLE <- mean(w.lo, w.hi)
    p.MLE <- 0.5
  }
 
  start <- c(log(theta.MLE)            # threshold 
             , rep(zero, ncovars-1)    # any covars
             , p.MLE                   # p = proportion below theta
             , rep(zero, expan))       # any expansions
  
  if( ncovars <= 1 ){
    # (Intercept)-only model. Use tighter bounds.
    low <- log(w.lo + fuzz)
    high <- log(w.hi - fuzz)
  } else {
    # We have covariates
    low <- rep(negInf, ncovars)
    high <- rep(posInf, ncovars)
  }
  # Add bounds for expansions
  low <- c(low
         , zero
         , rep(negInf, expan)
           )
  high  <- c(high
           , 1 - fuzz
           , rep(posInf, expan)
           )
  nms <- c(colnames(X), "p")
  
  if(expan > 0){
    nms <- c(nms, paste0( "a", 1:expan))
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}