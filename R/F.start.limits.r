#' @name F.start.limits
#' @aliases F.start.limits
#' @title F.start.limits - Set starting values and limits for parameters of Rdistance functions.
#' @description Return reasonable starting values and limits (boundaries) for the parameters of 
#'   distance functions.  Starting values and limits are specified for 
#'   all likelihoods and expansion terms.  This function is called by 
#'   other routines in \code{Rdistance}, and is not intended to 
#'   be called by the user.
#' @usage F.start.limits(like, expan, w.lo, w.hi, dist, covars = NULL, pointSurvey = FALSE)
#' @param like String specifying the likelihood for the distance function.  Possible values are 
#'   "hazrate" for hazard rate likelihood, "halfnorm" for the half 
#'   normal likelihood, "uniform" for the uniform likelihood, 
#'   "negexp" for the negative exponential likelihood, and 
#'   "Gamma" for the gamma likelihood.
#' @param expan Number of expansion terms to include. Valid values are 0, 1, ..., 3.
#' @param w.lo Lower or left-truncation limit of the distances.  Normally, 0.
#' @param w.hi Upper or right-truncation limit of the distances. This is the maximum off-transect distance that could be observed.
#' @param dist The vector of observed off-transect distances being analyzed.  This vector is only required for \code{like} = "Gamma" and "halfnorm".
#' @param covars Matrix of covariate values.
#' @param pointSurvey Boolean. TRUE if point transect data, FALSE if line transect data.
#' @details The number of parameters to be fitted is 
#'   \code{expan + 1 + 1*(like \%in\% c("hazrate", "uniform"))}.
#'   This is the length of all vectors returned in the output list.
#' @return A list containing the following components
#'   \item{start}{Vector of reasonable starting values for parameters of the likelihood and expansion terms. }
#'   \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
#'   \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
#'   \item{names}{Vector of names for the likelihood parameters and expansion terms.}
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#' @seealso \code{\link{dfuncEstim}}
#' @examples F.start.limits( "uniform", 0, 0, 1000 )
#'   F.start.limits( "uniform", 1, 0, 1000 )
#'   F.start.limits( "uniform", 2, 0, 1000 )
#'   F.start.limits( "uniform", 3, 0, 1000 )
#'   
#'   F.start.limits( "halfnorm", 0, 0, 1000, 500*runif(100) )
#'   F.start.limits( "halfnorm", 1, 0, 1000, 500*runif(100) )
#'   F.start.limits( "halfnorm", 2, 0, 1000, 500*runif(100) )
#'   F.start.limits( "halfnorm", 3, 0, 1000, 500*runif(100) )
#'   
#'   F.start.limits( "hazrate", 0, 0, 1000 )
#'   F.start.limits( "hazrate", 1, 0, 1000 )
#'   F.start.limits( "hazrate", 2, 0, 1000 )
#'   F.start.limits( "hazrate", 3, 0, 1000 )
#'   
#'   F.start.limits( "negexp", 0, 0, 1000 )
#'   F.start.limits( "negexp", 1, 0, 1000 )
#'   F.start.limits( "negexp", 2, 0, 1000 )
#'   F.start.limits( "negexp", 3, 0, 1000 )
#'   
#'   F.start.limits( "Gamma", 0, 0, 1000, 1000*runif(100) )
#' @keywords models
#' @export

F.start.limits <- function( like, expan, w.lo, w.hi, dist, covars = NULL, pointSurvey = FALSE ){
  #
  #   Establish starting value for parameters, and limits passed to the optimizer
  #
  
  #   Number of parameters
  if(!is.null(covars)){
  ncovars <- ncol(covars)
  }else{ncovars <- 1}
  
  np <- expan + 1*(like %in% c("hazrate","uniform")) + ncovars
  
  w <- w.hi - w.lo
  
  
  #   No starting values given
  if( like == "hazrate" ){
    if( ncovars > 1 ){
      start <- c(log(.5*w), rep(0, ncovars-1), 1,rep(0, expan))
      low   <- c(-10, rep(-Inf, ncovars-1), .01, rep(-Inf, expan))
    }
    else{
      start <- c(.5*w, 1,rep(0, np - 2))
      low   <- c(0, rep(-Inf, ncovars-1), .01, rep(-Inf, expan))
    }
    high  <- c(Inf, rep( Inf, ncovars-1), Inf, rep( Inf, expan))
    if( ncovars > 1 )
      nms <- c(colnames(covars), "Beta")
    else  
      nms <- c("Sigma", "Beta")
    if(expan > 0) nms <- c(nms, paste( "a", 1:expan, sep=""))
    
  } else if( like == "halfnorm" ){
    if( ncovars > 1 ){
      start <- c(log(sqrt(sum( (dist - w.lo)^2 )/length(dist))), rep(0, np - 1))
      low <- c(rep(-Inf, np))
    }
    else if(pointSurvey){
      start <- c(sqrt(sum( (dist - w.lo)^2 )/length(dist)), rep(0, np - 1))
      low <- c(0, rep(-Inf, np - 1 ))
    }
    else{
      start <- c(sqrt(sum( (dist - w.lo)^2 )/length(dist)), rep(0, np - 1))
      low <- c(0, rep(-Inf, np - 1 ))
    }
    high  <- c(Inf, rep( Inf, np - 1 ))
    if( ncovars > 1 )
      nms <- colnames(covars)
    else
      nms <- c("Sigma")
    if(expan > 0) nms <- c(nms, paste( "a", 1:(np-ncovars), sep=""))
    
  } else if( like == "uniform" ){
    if( ncovars > 1 ){
      start <- c(log(.1*w), rep(0, ncovars-1), 1, rep(0, expan))
      low   <- c(-14, rep(-Inf, ncovars-1), 0, rep(-Inf, expan))
    }
    else{
      start <- c(.1*w, 1, rep(0, np - 2))
      low   <- c(1e-6, rep(-Inf, ncovars-1), 0, rep(-Inf, expan))
    }
    high  <- c(w, rep(Inf, ncovars-1), Inf, rep( Inf, expan))
    if( ncovars > 1 )
      nms <- c(colnames(covars), "Knee")
    else
      nms <- c("Threshold", "Knee")
    if(expan > 0) nms <- c(nms, paste( "a", 1:expan, sep=""))
    
  } else if( like == "negexp" ){
    if( ncovars > 1 ){
      start <- c(0, rep(0, np - 1))
      low   <- c(-Inf, rep(-Inf, np - 1 ))
    }
    else{
      start <- c(1, rep(0, np - 1))
      low   <- c(0, rep(-Inf, np - 1 ))
    }
    high  <- c(Inf, rep( Inf, np - 1 ))
    if( ncovars > 1 )
      nms <- colnames(covars)
    else
      nms <- c("Beta")
    if(expan > 0) nms <- c(nms, paste( "a", 1:expan, sep=""))
    
    
  } else if( like == "Gamma" ){
    d <- dist[ w.lo <= dist & dist <= w.hi ]
    d <- d[ d > 0 ] # even though 0 is legit, can't take log of it
    s <- log( mean(d, na.rm=TRUE) ) - mean( log(d), na.rm=TRUE )
    s2 <- (s-3)^2 + 24*s
    if( s2 < 0 ) s2 <- 0
    r <- (3 - s + sqrt( s2 )) / (12*s)
    if( r <= 1 ) r <- 1.01
    b <- ( (r-1) / exp(1) )^(r-1) / gamma(r)
    lam <- mean(d,na.rm=TRUE) / (r * b)
    
    if( ncovars > 1 ){
      start <- c(log(r), rep(0, ncovars-1), lam)
      low   <- c(log(1.0001), rep(-Inf, ncovars-1), 0)
      nms <- colnames(covars)
    }
    else{
    start <- c(r, lam)
    low   <- c(1.0001, 0)
    nms <- c("Shape", "Scale")
    }
    high  <- c(Inf, Inf)
    
  } else {
    #   Assume this is a user-defined likelihood
    fn <- match.fun( paste(like, ".start.limits", sep="") )
    ans <- fn(dist, expan, w.lo, w.hi)
    start <- ans$start
    low <- ans$lowlimit
    high <- ans$highlimit
    nms <- ans$names
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}
