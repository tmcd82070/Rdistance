#' @title logistic.start.limits - Start and limit values for logistic distance function
#' 
#' @description Starting values and limits for parameters of the logistic 
#' distance function. 
#' 
#' @inheritParams logistic.like
#' 
#' @return A list containing the following components:
#' \itemize{
#'    \item \code{start} : a vector of starting values
#'    \item \code{lowlimit} : a vector of lower limits (can be -Inf)
#'    \item \code{highlimit} : a vector of upper limits (can be Inf)
#'    \item \code{nms} : a vector containing names of the parameters
#' }
#' 
#' @details This function is usually called within 
#' \code{F.start.limits}. 
#' 
#' @export
#' 
logistic.start.limits <- function(dist
                                , covars
                                , expan
                                , w.lo
                                , w.hi
                                ){

  # Dist, w.lo, and w.hi should all have units, or none have units
  # dist should already be reduced to values within w.lo to w.hi, 
  # but just in case...
  ind <- (w.lo <= dist) & (dist <= w.hi)  # unit conversions happen if needed
  dist <- dist[ind]
  negInf <- -.Machine$double.xmax / 100
  posInf <- -negInf
  zero <- 0
  if(!is.null(covars)){
    ncovars <- ncol(covars)
  } else { 
    ncovars <- 1
  }
  
  # fit logistic to counts as initial guess
  d <- units::drop_units(dist)
  distHist <- hist( d, plot = FALSE )
  distHist <- data.frame(counts = distHist$counts
                         , dist = distHist$mids
                         , n = max(distHist$counts))
  distFit <- stats::glm(counts / n ~ dist
                        , family = binomial
                        , weights = n
                        , data = distHist)
  coefFit <- coefficients(distFit)
  varCoefFit <- diag(summary(distFit)$cov.scaled) 
  covCoefFit <- summary(distFit)$cov.scaled[1,2]
  beta0 <- coefFit["(Intercept)"]
  beta1 <- coefFit["dist"]
  a.start <- -beta0 / beta1
  b.start <- -beta1
  # var.a <- (a.start^2) * ( (varCoefFit["(Intercept)"] / (beta0^2)) -
  #                            (2 * covCoefFit / (beta0 * beta1)) +
  #                            (varCoefFit["dist"] / (beta1^2)) )
  # if( var.a < 0 ){
  #   # not sure this can happen
  #   a.start <- -1 # using median in this case
  # } else {
  #   sd.a <- sqrt(var.a)
  # }
  
  a.low <- max(1e-7, 0.5 * quantile(d, p = 0.00)) 
  a.high <- quantile(d, p = 1.00) + 0.5 * sd(d)
  if( a.start <= 0 ){
    a.start <- median(d)
  } 
  if(b.start <= 0){
    b.start <- 1
    b.low <- 1e-7
    b.high <- 1e7
  } else {
    b.low <- max(1e-7, b.start - 1000*sqrt(varCoefFit["dist"]))
    b.high <- min(1e7, b.start + 1000*sqrt(varCoefFit["dist"]))
  }
  
  if( ncovars > 1 ){
    start <- c(log(a.start)                          # Threshold 
               , rep(zero, ncovars-1)                # Covars
               , b.start                             # Knee (no link fn)
               , rep(zero, expan))                   # any expansions
    low   <- c(log(a.low)
               , rep(negInf, ncovars-1)
               , b.low
               , rep(negInf, expan))
    high  <- c(log(a.high)
               , rep( posInf, ncovars-1)
               , b.high
               , rep( posInf, expan))
    nms <- c(colnames(covars), "Knee")
  } else {
    start <- c( a.start
               , b.start
               , rep(zero, expan))
    low   <- c(a.low
               , b.low 
               , rep(negInf, expan))
    high  <- c(a.high
               , b.high
               , rep( posInf, expan))
    nms <- c("Threshold", "Knee")
    
  }

  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }

  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}