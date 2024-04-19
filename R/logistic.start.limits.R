#' @title logistic.start.limits - Start and limit values for logistic distance function
#' 
#' @description Starting values and limits for parameters of the logistic 
#' distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @export
#' 
logistic.start.limits <- function(ml){

  X <- model.matrix(ml)
  dist <- Rdistance::distances(ml)  
  
  ncovars <- Rdistance:::nCovars(X)
  expan <- ml$expansions
  
  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  # fit logistic to counts as initial guess
  d <- units::set_units(dist, NULL)
  distHist <- hist( d, plot = FALSE )
  if(length(distHist$breaks) >= 4){
    distHist <- data.frame(counts = distHist$counts
                           , dist = distHist$mids
                           , n = max(distHist$counts))
    distFit <- stats::glm(counts / n ~ dist
                          , data = distHist
                          , family = stats::binomial
                          , weights = distHist$n
                          )
    coefFit <- stats::coefficients(distFit)
    beta0 <- coefFit[1]
    beta1 <- coefFit[2]
    a.start <- -beta0 / beta1
    b.start <- -beta1
    se.b.start <- summary(distFit)$coefficients["dist", "Std. Error"]
  } else {
    a.start <- stats::mean(d, na.rm = TRUE)
    b.start <- 1
    se.b.start <- 0.3
  }
  # varCoefFit <- diag(summary(distFit)$cov.scaled) 
  # covCoefFit <- summary(distFit)$cov.scaled[1,2]
  # var.a <- (a.start^2) * ( (varCoefFit["(Intercept)"] / (beta0^2)) -
  #                            (2 * covCoefFit / (beta0 * beta1)) +
  #                            (varCoefFit["dist"] / (beta1^2)) )
  # if( var.a < 0 ){
  #   # not sure this can happen
  #   a.start <- -1 # using median in this case
  # } else {
  #   sd.a <- sqrt(var.a)
  # }
  
  a.low <- max(fuzz, 0.5 * min(d, na.rm = TRUE)) 
  a.high <- max(d, na.rm = TRUE) 
  if( a.start <= zero ){
    a.start <- stats::mean(d, na.rm = TRUE)
  } 
  if(b.start <= fuzz){
    b.start <- 10*fuzz
  }
  b.low <- max(fuzz, b.start - 20*se.b.start)
  b.high <- b.start + 20*se.b.start

  if( ncovars > 1 ){
    start <- c(log(a.start)                          # Threshold 
               , rep(zero, ncovars-1)                # Covars
               , b.start                             # Knee (no link fn)
               , rep(zero, expan))              # any expansions
    low   <- c(negInf
               , rep(negInf, ncovars-1)
               , b.low
               , rep(negInf, expan))
    high  <- c(posInf
               , rep( posInf, ncovars-1)
               , b.high
               , rep( posInf, expan))
  } else {
    start <- c( log(a.start)
               , b.start
               , rep(zero, expan))
    low   <- c(log(a.low)
               , b.low 
               , rep(negInf, expan))
    high  <- c(log(a.high)
               , b.high
               , rep( posInf, expan))
  }
  nms <- c(colnames(X), "Knee")
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, low=low, high=high, names=nms )
  
}