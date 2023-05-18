#' @title Compute double observer probability of detection (No external covariates allowed)
#' 
#' @description Estimates the probability of detection in a two-observer system when observations are independent.
#' 
#' @param df A data frame containing the components \code{$obsby.1} and \code{$obsby.2}.
#'   These components are either 0/1 (0 = missed, 1 = seen) or TRUE/FALSE (logical) vectors indicating whether
#'   observer 1 (\code{obsby.1}) or observer 2 (\code{obsby.2}) spotted the target. There is 
#'   no flexibility 
#'   on naming these columns of \code{df}.  They must be named \code{$obsby.1} and \code{$obsby.2}.
#' @param observer A number of text string indicating the primary observer.  Primary observers can be 
#'   observer 1, or observer 2, or "both".
#'   If, for example, observer 2 was a data recorder and part-time observer, or if observer 2
#'   was the pilot, set \code{observer} = 1.  This dictates which set of observations form the denominator
#'   of the double observer system.  For example, if \code{observer} = 1, observations by observer 1 that were not seen
#'   by observer 2 are ignored. The estimate in this case uses targets seen by both observers and
#'   those seen by observer 2 but not observer 1. If observer = "both", the denominator is computed twice, once 
#'   assuming observer 1 was the primary, once assuming observer 2 was the primary, and then computes 
#'   the probability of one or more observers sighting a target.
#' @details When \code{observer} = "both", the observers are assumed to be independent. In this case the estimate 
#'   of detection is 
#'   \deqn{p = p_1 + p_2 - p_1p_2}{p = p1 + p2 - p1*p2}  
  #'   where \eqn{p_1}{p1} is the proportion of targets seen by observer 2 that were also seen by observer 1, 
#'   \eqn{p_2}{p2} is the proportion of targets seen by observer 1 that were also seen by observer 2.
#'   This estimator is very close to unbiased when observers are actually independent.
#' @return A single scalar, the probability of detection estimate.
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' 
#' @examples 
#' #   Fake observers
#'   set.seed(538392)
#'   obsrv <- data.frame( obsby.1=rbinom(100,1,.75), obsby.2=rbinom(100,1,.5) )
#'   
#'   F.double.obs.prob( obsrv, observer=1 )
#'   F.double.obs.prob( obsrv, observer=2 )
#'   F.double.obs.prob( obsrv, observer="both" )
#' @keywords model
#' @export

F.double.obs.prob <- function( df, observer = "both" ){
#
#   Compute the probability of detection from a double observer system.
#   No external covariates allowed here.
#
#   Inputs:
#   df = data frame containing the components $obsby.1, $obsby.2.
#       These components are TRUE/FALSE (logical) vectors indicating whether
#       observer 1 (obsby.1) or observer 2 (obsby.2) spotted the target.
#   observer = indicates whether observer 1 or observer 2 or both were full-time observers.
#       If, for example, observer 2 was a data recorder and part-time observer, or if observer 2
#       was the pilot, set "observer = 1".  This dictates which set of observations form the denominator
#       of the double observer system.  For example, if "observer = 1", observations by observer 1 that were not seen
#       by observer 2 are ignored. The estimate in this case uses targets seen by both observers and
#       those seen by observer 2 but not observer 1. If observer = "both", the computation goes both directions.

if( !(all(c("obsby.1", "obsby.2") %in% names(df)))){
    stop("Variables 'obsby.1' and 'obsby.2' not found in input data frame.")
}

obs1 <- as.logical( df$obsby.1 )
obs2 <- as.logical( df$obsby.2 )
obs.both  <-  obs1 & obs2

if( is.character( observer ) ){
    if(observer == "both"){
        obs.tot  <-  obs1 | obs2  # this should be all 1's, assuming no extra lines are input (like "unknown")
        p1 <- sum( obs.both ) / sum( obs2 )
        p2 <- sum( obs.both ) / sum( obs1 )
        # Assume observers are independent here.  This was checked via simulation. This estimator is close to unbiased when observers are actually independent.
        p <- p1 + p2 - p1*p2
    } else {
        stop("Inappropriate 'observer' parameter")
    }
} else if( observer == 1 ){
    p <- sum( obs.both ) / sum( obs2 )
} else if( observer == 2 ){
    p <- sum( obs.both ) / sum( obs1 )
} else {
    stop("Inappropriate 'observer' parameter")
}

p

}        
        
