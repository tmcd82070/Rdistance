#' @title expansionTerms - Distance function expansion terms
#' 
#' @description
#' Compute "expansion" terms that modify the shape of 
#' a base distance function. 
#' 
#' @param a A vector or matrix of (estimated) coefficients. 
#' \code{a} has length \eqn{p} + \code{nexp} (if a vector) or dimension
#' (\eqn{k}, \eqn{p} + \code{nexp}), where \eqn{p} is the number 
#' of canonical parameters in the likeihood and \eqn{k} is the 
#' number of coefficient vectors to evaluate. The first \eqn{p}
#' elements of \code{a}, or the first \eqn{p} columns if \code{a}
#' is a matrix, are ignored. I.e., Expansion 
#' term coefficients are the last \code{nexp} elements or columns 
#' of \code{a}. 
#'
#' @param d A vector or 1-column matrix of 
#' distances at which to evaluate 
#' the expansion terms.  \code{d} should be distances 
#' above w.lo, i.e., distances - w.lo. 
#' Parameters \code{d} and \code{w}
#' must have compatible measurement units.
#' 
#' @param nexp Number of expansion terms.  Integer from 0 to 5. 
#'
#' @param w Strip width, i.e., \code{w.hi} - \code{w.low} = 
#' range of \code{d}. Parameters \code{d} and \code{w} must 
#' have compatible measurement units.
#' 
#' @inheritParams dE.single 
#' 
#' @details
#' Expansion terms modify the "key" function of the 
#' likelihood multiplicatively.  The modified distance function is,
#' \code{key * expTerms} where \code{key} is a vector of values in
#' the base likelihood function (e.g., \code{halfnorm.like()$L.unscaled} 
#' or \code{hazrate.like()$L.unscaled}) and \code{expTerms} is the 
#' matrix returned by this routine. 
#' 
#' Let the number of expansions (\code{nexp}) be \eqn{m} (\eqn{m} > 0),
#' assume the raw cyclic expansion terms of \code{series} 
#' are \eqn{h_{j}(x)}{h_j(x)} for 
#' the \eqn{j^{th}}{j-th} expansion of distance \eqn{x}, and that 
#' \eqn{a_1, a_2, \dots, a_m}{a(1), a(2), ..., a(m)} are (estimated) 
#' coefficients for the expansion terms, then the likelihood contribution 
#' for the \eqn{i^{th}}{i-th} distance \eqn{x_i}{x(i)} is, 
#'   \deqn{f(x_i|\beta,a_1,a_2,\dots,a_m) = 
#'    f(x_i|\beta)(1 + \sum_{k=1}^{m} a_k h_{k}(x_i/w)).}{f(x(i)|beta,a_1,a_2,...,a_k) = 
#'    f(x(i)|beta)(1 + a(1) h_1(x(i)/w) + a(2) h_2(x(i)/w) + ... + a(m) h_m(x(i)/w)). }
#' 
#' @return If \code{nexp} equals 0, 1 is returned. 
#' If \code{nexp} is greater than 0, a matrix of 
#' size \eqn{n}X\eqn{k} containing expansion terms, 
#' where \eqn{n} = \code{length(d)} and \eqn{k} = \code{nrow(a)}. The 
#' expansion series associated with row \eqn{j} of \code{a} 
#' are in column \eqn{j} of the return. i.e., 
#' element (\eqn{i},\eqn{j}) of the return is 
#'   \deqn{1 + \sum_{k=1}^{m} a_{jk} h_{k}(x_{i}/w).}{(1 + a(j1) h_1(x(i)/w) + ... + a(jm) h_m(x(i)/w)) }
#' (see Details).
#' @examples
#' a1 <- c(log(40), 0.5, -.5)
#' a2 <- c(log(40), 0.25, -.5)
#' dists <- units::set_units(seq(0, 100, length = 100), "m")
#' w = units::set_units(100, "m")
#' 
#' expTerms1 <- expansionTerms(a1, dists, "cosine", 2, w)
#' expTerms2 <- expansionTerms(a2, dists, "cosine", 2, w)
#' plot(dists, expTerms2, ylim = c(0,2.5))
#' points(dists, expTerms1, pch = 16)
#' 
#' # Same as above
#' a <- rbind(a1, a2)
#' expTerms <- expansionTerms(a, dists, "cosine", 2, w)
#' matlines(dists, expTerms, lwd=2, col=c("red", "blue"), lty=1)
#' 
#' # Showing key and expansions
#' key <- halfnorm.like(log(40), dists, 1)$L.unscaled
#' plot(dists, key, type = "l", col = "blue", ylim=c(0,1.5))
#' lines(dists, key * expTerms1, col = "red")
#' lines(dists, key * expTerms2, col = "purple")
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
      stop( paste( "Unknown expansion series", series, "requested." ))
    }

    if(is.matrix(a)){
      coefLocs <- (ncol(a)-(nexp-1)):(ncol(a))
      expCoeffs <- a[, coefLocs, drop = FALSE]  # k X q
    } else {
      coefLocs <- (length(a)-(nexp-1)):(length(a))
      expCoeffs <- matrix(a[coefLocs], nrow = 1) # 1 X q
    }
    
    expTerms <- (1 + exp.term %*% t(expCoeffs)) # (nXk)
    
    # Standardize the terms using the following statements:
    # expTerms <- expTerms / (1 + rowSums(expCoeffs))
    # OR, expCoeffs <- expCoeffs / rowSums(expCoeffs) # sum to 1
    #     expTerms <- (1 + c(exp.term %*% t(expCoeffs))) 
    
  } else {  
    expTerms <- 1
  }
  
  expTerms
  
}