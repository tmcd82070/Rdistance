#' @title expansionTerms - Compute distance function expansion terms
#' 
#' @description
#' Computes one of several types of "expansion" terms that 
#' that can multiplied into a distance function likelihoods. 
#' 
#' @inheritParams nLL
#'
#' @param d A vector of distances at which to evaluate the expansion 
#' terms. This vector should be distances above w.lo, i.e., if w.lo > 0, 
#' this vector should be (distances - w.lo). Parameters \code{d} and \code{w}
#' must have compatible measurement units.
#' 
#' @param nexp Number of expansion terms.  Integer from 0 to 5. 
#'
#' @param w Strip width, i.e., range of \code{d} is 0 to \code{w}.
#' Parameters \code{d} and \code{w} must have compatible measurement units.
#' 
#' @details
#' Expansion terms modify the "key" function of the 
#' likelihood multiplicatively.  The modified distance function is,
#' \code{key * expTerms} where \code{key} is a vector of values in
#' the base distance function (e.g., \code{halfnorm.like()$L.unscaled} or 
#' \code{hazrate.like()$L.unscaled}) and \code{expTerms} is the vector returned
#' by this routine. 
#' 
#' @section Expansion Terms: 
#' 
#' If the number of expansions (\code{nexp}) is k, k is greater 
#' than 0, and assuming 
#' the expansion terms specified in \code{series} 
#' are \eqn{h_{ij}(x)}{h_ij(x)} for 
#' the \eqn{j^{th}}{j-th} expansion of the \eqn{i^{th}}{i-th} 
#' distance, and that 
#' \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} are (estimated) 
#' coefficients for the expansion terms, the likelihood contribution 
#' for the \eqn{i^{th}}{i-th} distance is, 
#'   \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = 
#'    f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(d/w)).}
#'    {f(x|a,b,c_1,c_2,...,c_k) = 
#'    f(x|a,b)(1 + c(1) h_i1(d/w) + c(2) h_i2(d/w) + ... + c(k) h_ik(d/w)). }
#' 
#' @return A vector of the expansion terms.  These are
#' the cyclic expansion terms multiplied by expansion term 
#' coefficients in \code{a}.
#' 
#' @examples
#' a1 <- c(log(40), 0.5, -.5)
#' a2 <- c(log(40), 0.25, -.5)
#' dists <- units::set_units(seq(0, 100, length = 100), "m")
#' w = units::set_units(100, "m")
#' 
#' expTerms1 <- expansionTerms(a1, dists, "cosine", 2, w)
#' expTerms2 <- expansionTerms(a2, dists, "cosine", 2, w)
#' plot(dists, expTerms1 / (1 + sum(a1[2:3])))
#' points(dists, expTerms2 / (1 + sum(a2[2:3])), pch = 16)
#' 
#' # Showing key and expansions
#' key <- halfnorm.like(log(40), dists, 1)$L.unscaled
#' plot(dists, key, type = "l", col = "blue", ylim=c(0,1.5))
#' lines(dists, key * expTerms1 / sum(a1[-1]), col = "red")
#' lines(dists, key * expTerms2 / sum(a2[-1]), col = "purple")
#' 
#' @export
#'    
expansionTerms <- function(a, d, series, nexp, w){
  
  if( nexp > 0 ){
    dscl <- units::set_units(d/w, NULL)   # unit conversion here; drop units is safe
    
    if (series=="cosine"){
      exp.term <- cosine.expansion( dscl, nexp )
    } else if (series=="hermite"){
      # dscl <- units::drop_units(dist/sigma) # not sure /sigma matters; I think we can use /w
      exp.term <- hermite.expansion( dscl, nexp )
    } else if (series == "simple") {
      exp.term <- simple.expansion( dscl, nexp )
    } else {
      stop( paste( "Unknown expansion series. Found", ml$series ))
    }

    expCoeffs <- a[(length(a)-(nexp-1)):(length(a))]
    expTerms <- (1 + c(exp.term %*% expCoeffs)) # / (1 + sum(expCoeffs))
  } else {  
    expTerms <- 1
  }
  
  expTerms
  
}