#' @title Huber Cumulative Function
#' 
#' @description
#' Computes the cumulative function of the `huber` likelihood. The 
#' cumulative function is proportional to the `huber` 
#' cumulative distribution function, differing only by appropriate
#' scaling constant. 
#' 
#' @param x A vector of distances. Can have units or not
#' (i.e., regular numeric). 
#' 
#' @param t1 A vector of values for the \eqn{\theta_1}{T1} parameter 
#' of the `huber` likelihood. If \code{x} has units, \code{t1} must have 
#' compatible units.
#' 
#' @param t2 A vector of values for the \eqn{\theta_2}{T2} parameter 
#' of the `huber` likelihood. If \code{x} has units, \code{t2} must have 
#' compatible units.
#' 
#' @param p A vector of values for the \eqn{p} parameter of the 
#' `huber` likelihood. If \code{x} has units, \code{p} must have 
#' units of '[1]' (i.e., \code{setUnits(p,1)}). 
#' 
#' @param w A vector of maximum strip widths, the maximum distance. 
#' If \code{x} has units, \code{x} must have 
#' compatible units.
#'  
#' @return A vector of values from the `huber` cumulative function. 
#' The `huber` cumulative function is
#' \deqn{
#' F(x|\theta_1,\theta_2,p) = \int_0^x f(y|\theta_1,\theta_2,p) dy,}{
#' F(x|T1,T2,p) = Area Under f(y|T1,T2,p) from 0 to x,
#' }
#' where \eqn{f(y|\theta_1,\theta_2,p)}{f(y|T1,T2,p)} is Rdistance's `huber` 
#' likelihood. The only difference between this \emph{cumulative function}, 
#' and the \emph{cumulative distribution function}
#' is the scaling constant. That is, the maximum of the \emph{cumulative 
#' function} is greater than 1 while the maximum \emph{cumulative distribution function}
#' is exactly 1.
#' 
#' @seealso \code{\link{huber.like}}
#' 
#' @examples 
#' d <- -10:210
#' 
#' # Cumulative function
#' fd <- huber.cumFunc(d, 125, 25, .05, 200) 
#' plot(d, fd, type="l")
#' 
#' # Cumulative distribution function
#' Fd <- fd / huber.cumFunc(200, 125, 25, .05, 200)
#'
#' @export 
huber.cumFunc <- function(x, t1, t2, p, w){
  if( inherits(x, "units") ){
    zero <- setUnits(0, units(x))
    one <- setUnits(1, "1")
  } else {
    zero <- 0
    one <- 1
  }
  
  # All vectors must be same length due to subsetting used below
  n <- length(x)
  if(length(t1) == 1){
    t1 <- rep(t1, n)
  } else if(length(t1) != n){
    stop(paste0("Length of t1 vector must be 1 or same as x, which is length ", n, ". "
                , "t1 is length ", length(t1), "."))
  }
  if(length(t2) == 1){
    t2 <- rep(t2, n)
  } else if(length(t2) != n){
    stop(paste0("Length of t2 vector must be 1 or same as x, which is length ", n, ". "
              , "t2 is length ", length(t2), "."))
  }
  if(length(p) == 1){
    p <- rep(p, n)
  } else if(length(p) != n){
    stop(paste0("Length of p vector must be 1 or same as x, which is length ", n, ". "
                , "p is length ", length(p), "."))
  }
  if(length(w) == 1){
    w <- rep(w, n)
  } else if(length(w) != n){
    stop(paste0("Length of w vector must be 1 or same as x, which is length ", n, ". "
                , "w is length ", length(w), "."))
  }

  # Initiation
  out <- rep(zero, length(x))
  greater0 <- x > zero
  
  x <- pmin(x, w)
  
  T2 <- t1 + t2
  m <- t1*(T2 - 0.5*t1)
  K <- (one - p) / (2*m)

  # Part 1: everyone over 0  
  x1 <- pmin(x, t1)[greater0]
  KGT <- K[greater0]
  out[greater0] <- x1 - KGT * (x1^3 / 3)
  
  # Part 2: t1 < x < T2; but x=min(x,w) 
  greaterT1 <- x > t1
  x2 <- pmin(x, T2)[greaterT1]
  tGT <- t1[greaterT1]
  KGT <- K[greaterT1]
  gAtT1 <- one - KGT * tGT^2
  gAtX  <- one - KGT * tGT * (2*x2 - tGT) #(1-p)*(t1*(x - 0.5*t1))/m
  f2 <- (x2 - tGT) * (gAtT1 + gAtX) / 2 # trapazoid, t1 to x
  out[greaterT1] <- out[greaterT1] + f2
  
  # Part 3: T2 < x < w
  greaterT2 <- x > T2
  f3 <- (x[greaterT2] - T2[greaterT2])*p[greaterT2]
  out[greaterT2] <- out[greaterT2] + f3
  
  return(out)
  
}
