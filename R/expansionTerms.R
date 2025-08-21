#' @title Distance function expansion terms
#' 
#' @description
#' Compute "expansion" terms that modify the shape of 
#' a base distance function. 
#' 
#' @param a A vector or matrix of (estimated) coefficients. 
#' \code{a} has length \eqn{p} + \code{nexp} (if a vector) or dimension
#' (\eqn{k}, \eqn{p} + \code{nexp}), where \eqn{p} is the number 
#' of canonical parameters in the likelihood and \eqn{k} is the 
#' number of 'cases' (coefficient vectors = \code{nrow(a)}) to evaluate. 
#' The first \eqn{p}
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
#' @param w A vector specifying strip width for every 'case' 
#' in \code{a}.  Vector must have length \code{length(a)} or \code{nrow(a)}. 
#' In general, this is constant vector containing the range 
#' of sighting distances, i.e., \code{rep(w.hi - w.low, nrow(a))}. 
#' But, for some likelihoods (e.g., 'oneStep') this vector allows the user to 
#' restrict application of the expansion terms to less than the full range 
#' of distances. For the 'oneStep' likelihood, expansion terms are only 
#' applied between 0 and \eqn{\Theta}{T}, the boundary of the two uniforms, 
#' which varies by 'case' when covariates are present.  
#' Parameters \code{d} and \code{w} must 
#' have compatible measurement units.
#' 
#' @inheritParams dE.single 
#' 
#' @details
#' Expansion terms modify the base likelihood function
#' and are used to incorporate "wiggle".  The modified distance function is,
#' \code{key * expTerms} where \code{key} is a vector of values in
#' the base likelihood function (e.g., \code{halfnorm.like()$L.unscaled}) 
#' and \code{expTerms} is the 
#' matrix returned by this routine. In equation form, 
#'   \deqn{f(x_i|\beta,a_1,a_2,\dots,a_m) = 
#'    f(x_i|\beta)(1 + \sum_{k=1}^{m} a_k h_{k}(x_i/w)).}{f(x(i)|beta,a_1,a_2,...,a_k) = 
#'    f(x(i)|beta)(1 + a(1) h_1(x(i)/w) + a(2) h_2(x(i)/w) + ... + a(m) h_m(x(i)/w)). },
#' where \eqn{m} = the the number of expansions (\code{nexp}), \eqn{h_{j}(x)}{h_j(x)} 
#' are expansion terms for distance \eqn{x}, and  
#' \eqn{a_1, a_2, \dots, a_m}{a(1), a(2), ..., a(m)} are the (estimated) 
#' expansion term coefficients.
#' 
#' @return If \code{nexp} equals 0, 1 is returned. 
#' If \code{nexp} is greater than 0, a matrix of 
#' size \eqn{n}X\eqn{k} containing expansion terms, 
#' where \eqn{n} = \code{length(d)} and \eqn{k} = \code{nrow(a)}. The 
#' expansion series associated with row \eqn{j} of \code{a} 
#' are in column \eqn{j} of the return. i.e., 
#' element (\eqn{i},\eqn{j}) of the return is 
#'   \deqn{1 + \sum_{k=1}^{m} a_{jk} h_{k}(x_{i}/w).}{(1 + a(j1) h_1(x(i)/w) + ... + a(jm) h_m(x(i)/w)).}
#' 
#' 
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
#' w <- rep(w, nrow(a))
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
    # dimensions:
    #  d = length(d) = number of distances
    #  n = length(w) 
    #  nexp = number of expasions
    # Note:
    #  nrow(a) == length(w) for all likes 
    
    dscl <- units::set_units(outer(d, w, "/"), NULL) # d X n
    
    indOutside <- dscl > 1  # save this for later

    if (series=="cosine"){
      exp.term <- cosine.expansion( dscl, nexp ) # returns d X n X nexp array
    } else if (series=="hermite"){
      # dscl <- units::drop_units(dist/sigma) # not sure /sigma matters; I think we can use /w
      exp.term <- hermite.expansion( dscl, nexp )
    } else if (series == "simple") {
      exp.term <- simple.expansion( dscl, nexp )
    } else {
      stop( paste( "Unknown expansion series", series, "requested." ))
    }

    if(is.matrix(a)){
      # I think a is always a matrix
      coefLocs <- (ncol(a)-(nexp-1)):(ncol(a))
      expCoeffs <- a[, coefLocs, drop = FALSE]  # n X nexp
    } else {
      coefLocs <- (length(a)-(nexp-1)):(length(a))
      expCoeffs <- matrix(a[coefLocs], nrow = 1) # 1 X nexp
    }
    
    jMat <- kronecker(diag(nexp), matrix(1, 1, nrow(exp.term))) # nexp X (nexp*d)
    bigCoeffs <- expCoeffs %*% jMat # n X nexp * nexp X (nexp*d) = n X (nexp*d)
    bigCoeffs <- array(bigCoeffs
                     , dim = c(nrow(expCoeffs), nrow(exp.term), nexp))
    bigCoeffs <- aperm(bigCoeffs, c(2,1,3)) # was nXdXnexp; now dXnXnexp; constant w/i pages
    expTerms <- 1 + apply(exp.term * bigCoeffs, c(1,2), sum) # apply(dXnXnExp) = dXn; sums across nexp

    expTerms[ indOutside ] <- 1 # blank out values > w

  } else {  
    expTerms <- 1
  }
  
  expTerms
  
}