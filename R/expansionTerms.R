#' @title expansionTerms - Compute distance function expansion terms
#' 
#' @description
#' Computes one of several times of "expansion" terms that 
#' modify distance functions. 
#' 
#' @inheritParams nLL
#' 
#' @inheritParams startLimits
#'
#' @details
#' Expansion terms modify the "key" function of the 
#' likelihood multiplicatively.  The modified distance function is,
#' \code{key * expTerms} where \code{key} is a vector of values in
#' the base distance function (e.g., \code{halfnorm.like} or 
#' \code{hazrate.like}) and \code{value} is the vector returned
#' by this routine. 
#' 
#' @return A vector of the expansion terms.  These are
#' the cyclic expansion terms multiplies by exansion term 
#' coefficients in \code{a}.
#' 
#' @examples
#' a1 <- c(log(40), 0.5, -.5)
#' a2 <- c(log(40), 0.25, -.5)
#' dists <- units::set_units(seq(0, 100, length = 100), "m")
#' 
#' ml <- list(
#'     mf = model.frame(dists ~ 1)
#'   , expansions = 2
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(100, "m")
#'   , series = "cosine"
#'   , likelihood = "halfnorm"
#' )
#' expTerms1 <- expansionTerms(a1, ml)
#' expTerms2 <- expansionTerms(a2, ml)
#' plot(dists, expTerms1)
#' points(dists, expTerms2, pch = 16)
#' 
#' # Showing key and expansions
#' key <- halfnorm.like(log(40), dists, 1)$key
#' plot(dists, key, type = "l", col = "blue", ylim=c(0,1.5))
#' lines(dists, key * expTerms1 / expTerms1[1], col = "red")
#' lines(dists, key * expTerms2 / expTerms2[1], col = "purple")
#' 
#' @export
#'    
expansionTerms <- function(a, ml){
  
  nexp <- ml$expansions
  if( nexp > 0 ){
    dist <- stats::model.response(ml$mf)
    w <- ml$w.hi - ml$w.lo  # 'w' has units here, we want this so conversions below happen
    dscl <- units::drop_units(dist/w)   # unit conversion here; drop units is safe
    
    if (ml$series=="cosine"){
      exp.term <- cosine.expansion( dscl, nexp )
    } else if (ml$series=="hermite"){
      # dscl <- units::drop_units(dist/sigma) # not sure /sigma matters; I think we can use /w
      exp.term <- hermite.expansion( dscl, nexp )
    } else if (ml$series == "simple") {
      exp.term <- simple.expansion( dscl, nexp )
    } else {
      stop( paste( "Unknown expansion series. Found", ml$series ))
    }

    expCoeffs <- a[(length(a)-(nexp-1)):(length(a))]
    expTerms <- 1 + c(exp.term %*% expCoeffs)
  } else {  
    expTerms <- 1
  }
  
  expTerms
  
}