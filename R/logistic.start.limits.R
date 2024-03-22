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
logistic.start.limits <- function(ml){

  X <- stats::model.matrix(ml$mt, ml$mf)
  dist <- stats::model.response(ml$mf)  
  
  ncovars <- ncol(X)
  expan <- ml$expansions
  
  fuzz <- getOption("Rdistance_fuzz")
  zero <- getOption("Rdistance_zero")
  posInf <- getOption("Rdistance_posInf")
  negInf <- getOption("Rdistance_negInf")
  
  # fit logistic to counts as initial guess
  d <- units::drop_units(dist)
  distHist <- hist( d, plot = FALSE )
  distHist <- data.frame(counts = distHist$counts
                         , dist = distHist$mids
                         , n = max(distHist$counts))
  distFit <- stats::glm(distHist$counts / distHist$n ~ distHist$dist
                        , family = stats::binomial
                        , weights = distHist$n
                        )
  coefFit <- stats::coefficients(distFit)
  varCoefFit <- diag(summary(distFit)$cov.scaled) 
  covCoefFit <- summary(distFit)$cov.scaled[1,2]
  beta0 <- coefFit[1]
  beta1 <- coefFit[2]
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
  
  a.low <- max(fuzz, 0.5 * quantile(d, p = 0.00)) 
  a.high <- quantile(d, p = 1.00) + 0.5 * stats::sd(d)
  if( a.start <= zero ){
    a.start <- stats::median(d)
  } 
  if(b.start <= fuzz){
    b.start <- 1
    b.low <- fuzz
    b.high <- fuzz
  } else {
    sdb2 <- varCoefFit[2]
    if(sdb2 <= zero){
      sdb2 <- 1
    } else {
      sdb2 <- sqrt(sdb2)
    }
    b.low <- max(fuzz, b.start - 1000*sdb2)
    b.high <- min(fuzz, b.start + 1000*sdb2)
  }
  
  if( ncovars > 1 ){
    start <- c(log(a.start)                          # Threshold 
               , rep(zero, ncovars-1)                # Covars
               , b.start                             # Knee (no link fn)
               , rep(zero, expan))              # any expansions
    low   <- c(log(a.low)
               , rep(negInf, ncovars-1)
               , b.low
               , rep(negInf, expan))
    high  <- c(log(a.high)
               , rep( posInf, ncovars-1)
               , b.high
               , rep( posInf, expan))
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
  }
  nms <- c(colnames(X), "Knee")
  
  if(expan > 0){
    nms <- c(nms, paste( "a", 1:expan, sep=""))
  }
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}