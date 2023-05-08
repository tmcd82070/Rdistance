#' @title Set starting values and limits for parameters of Rdistance functions
#' 
#' @description Return reasonable starting values and limits (boundaries) for the parameters of 
#'   distance functions.  Starting values and limits are specified for 
#'   all likelihoods and expansion terms.  This function is called by 
#'   other routines in \code{Rdistance}, and is not intended to 
#'   be called by the user.
#'   
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
#'  
#' @seealso \code{\link{dfuncEstim}}
#' @examples 
#'   data(sparrowDetectionData)
#'   dist <- sparrowDetectionData$dist
#'   units(dist) <- "m"
#'   wl <- units::as_units(0, "m")
#'   wh <- units::as_units(1000, "m")
#'   
#'   F.start.limits("uniform", 0, wl, wh, dist)
#'   F.start.limits("uniform", 1, wl, wh, dist)
#'   F.start.limits("uniform", 2, wl, wh, dist)
#'   F.start.limits("uniform", 3, wl, wh, dist)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist)
#'   F.start.limits("halfnorm", 1, wl, wh, dist)
#'   F.start.limits("halfnorm", 2, wl, wh, dist)
#'   F.start.limits("halfnorm", 3, wl, wh, dist)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 1, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 2, wl, wh, dist, pointSurvey = TRUE)
#'   F.start.limits("halfnorm", 3, wl, wh, dist, pointSurvey = TRUE)
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 1, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 2, wl, wh, dist, data.frame(A=1, B=2))
#'   F.start.limits("halfnorm", 3, wl, wh, dist, data.frame(A=1, B=2))
#'   
#'   F.start.limits("halfnorm", 0, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 1, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 2, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   F.start.limits("halfnorm", 3, wl, wh, dist, data.frame(A=1, B=2), TRUE)
#'   
#'   F.start.limits("hazrate", 0, wl, wh, dist)
#'   F.start.limits("hazrate", 1, wl, wh, dist)
#'   F.start.limits("hazrate", 2, wl, wh, dist)
#'   F.start.limits("hazrate", 3, wl, wh, dist)
#'   
#'   F.start.limits("negexp", 0, wl, wh, dist)
#'   F.start.limits("negexp", 1, wl, wh, dist)
#'   F.start.limits("negexp", 2, wl, wh, dist)
#'   F.start.limits("negexp", 3, wl, wh, dist)
#'   
#'   F.start.limits("Gamma", 0, wl, wh, dist)
#' @keywords models
#' @export
F.start.limits <- function( like
                          , expan
                          , w.lo
                          , w.hi
                          , dist
                          , covars = NULL
                          , pointSurvey = FALSE ){
  #
  #   Establish starting value for parameters, and limits passed to the optimizer
  #
  # Someday: Get rid of the special cases of no covariates. 
  
  #   Number of parameters
  if(!is.null(covars)){
    ncovars <- ncol(covars)
  } else { 
    ncovars <- 1
  }
  
  np <- expan + 1*(like %in% c("hazrate","uniform")) + ncovars
  
  zero <- 0.0
  negInf <- -Inf 
  posInf <- Inf 

  # Take max() just in case there are distances < w.lo; there should not be, but...
  # We want unit conversions here; e.g., dist in m, w.lo in ft
  dMin <- max( min(dist), w.lo )
  dMax <- min( max(dist), w.hi )
  w <- w.hi - w.lo
  medDist <- stats::median(dist)
  if( inherits(dist, "units") ){
    # Only time dist will not have units is when user overrides requirement
    # otherwise this always runs
    dMin <- units::drop_units(dMin)
    dMax <- units::drop_units(dMax)
    w <- units::drop_units(w)
    medDist <- units::drop_units(medDist)
  }
  
  #   No starting values given
  if( like == "hazrate" ){
    if( ncovars > 1 ){  
      start <- c(log(0.8 * medDist)   # Sigma 
                 , rep(0, ncovars-1)    # any covars
                 , 1               # k
                 , rep(0, expan))        # any expansions
      low   <- c(negInf
                 , rep(negInf, ncovars-1)
                 , 0.01
                 , rep(negInf, expan))
      high  <- c(posInf
                 , rep( posInf, ncovars-1)
                 , 100 
                 , rep( posInf, expan))
      nms <- c(colnames(covars), "k")
    } else {
      start <- c(0.8 * medDist      # Sigma 
                 , 1                # k
                 , rep(0, expan))   # any expansions
      low   <- c(dMin
                 , 0.01
                 , rep(negInf, expan))
      high  <- c(dMax
                 , 100
                 , rep( posInf, expan))
      nms <- c("Sigma", "k")
    }
      
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:expan, sep=""))
    }
    
  } else if( like == "halfnorm" ){
    sdHalf <- sqrt(sum( (dist - w.lo)^2 )/length(dist))
    if( inherits(sdHalf, "units") ){
      # Only time sdHalf will not have units is when user overides requirement
      # otherwise this always runs
      sdHalf <- units::drop_units(sdHalf)
    }
    
    if( ncovars > 1 ){
      start <- c(log(sdHalf)
               , rep(zero, np - 1))
      low <- rep(negInf, np)
      high  <- rep( posInf, np )
      nms <- colnames(covars)
    } else {
      start <- c(sdHalf
               , rep(zero, np - 1))
      low <- c(dMin
             , rep(negInf, np - 1 ))
      high  <- c(dMax
               , rep( posInf, np - 1 ))
      nms <- c("Sigma")
    } 
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:(np-ncovars), sep=""))
    }
    
  } else if( like == "negexp" ){
    if( ncovars > 1 ){
      start <- c(zero
               , rep(zero, np - 1))
      low   <- c(negInf
               , rep(negInf, np - 1 ))
      high  <- c(posInf
               , rep( posInf, np - 1 ))
      nms <- colnames(covars)
    } else {
      start <- c( 0.5*(0.5 / (log(medDist) - log(dMin))) + 0.5*(0.5/ (log(dMax) - log(medDist))), 
                 rep(zero, expan))
      low   <- c(0.01, 
                 rep(negInf, expan ))
      high  <- c(100*w
                 , rep( posInf, expan ))
      nms <- c("Beta")
    }
    if(expan > 0){
      nms <- c(nms, paste( "a", 1:expan, sep=""))
    }
  
  }  else {
    #   This is for logistic, Gamma, and all user-defined likelihoods
    # Eventually, all start limit functions should be their own routine
    fn <- match.fun( paste(like, ".start.limits", sep="") )
    ans <- fn(dist, covars, expan, w.lo, w.hi)
    start <- ans$start
    low <- ans$lowlimit
    high <- ans$uplimit
    nms <- ans$names
  }
  
  names(start) <- nms
  names(low) <- nms
  names(high) <- nms
  
  list( start=start, lowlimit=low, uplimit=high, names=nms )
  
}
