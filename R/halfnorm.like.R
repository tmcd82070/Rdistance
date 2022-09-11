#' @title Half-normal likelihood function for distance analyses
#' 
#' @description This function computes the likelihood contributions for 
#' sighting distances, scaled appropriately, for use as a 
#' distance likelihood.
#' 
#' @param a A vector of likelihood parameter values. Length and 
#' meaning depend on \code{series} and \code{expansions}. If no expansion 
#' terms were called for (i.e., \code{expansions = 0}), the distance 
#' likelihoods contain one or two canonical parameters (see Details). 
#' If one or more expansions are called for, coefficients for the 
#' expansion terms follow coefficients for the canonical parameters.  
#' i.e., if \code{p} is the number of canonical parameters, coefficients
#' for the expansion terms are \code{a[(p+1):length(a)]}.
#' 
#' @param dist A numeric vector containing the observed distances.
#' 
#' @param covars Data frame containing values of covariates at 
#' each observation in \code{dist}.
#' 
#' @param w.lo Scalar value of the lowest observable distance.  
#' This is the \emph{left truncation} of sighting distances 
#' in \code{dist}. Same units as \code{dist}. Values less than 
#' \code{w.lo} are allowed in \code{dist}, but are ignored and 
#' their contribution to the likelihood is set to \code{NA} in the output.
#' 
#' @param w.hi Scalar value of the largest observable distance.  
#' This is the \emph{right truncation} of sighting distances 
#' in \code{dist}.  Same units as \code{dist}. Values greater 
#' than \code{w.hi} are allowed in \code{dist}, but are ignored 
#' and their contribution to the likelihood is set to \code{NA} 
#' in the output.
#' 
#' @param series A string specifying the type of expansion to use.  
#' Currently, valid values are 'simple', 'hermite', and 'cosine'; but, see 
#'   \code{\link{dfuncEstim}} about defining other series.
#'   
#' @param expansions A scalar specifying the number of terms 
#' in \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#'   
#' @param scale Logical scalar indicating whether or not to 
#' scale the likelihood so it integrates to 1. This parameter is 
#' used to stop recursion in other functions. If \code{scale} 
#' equals TRUE, a numerical integration routine 
#' (\code{\link{integration.constant}}) is called, which in turn 
#' calls this likelihood function again with \code{scale} = FALSE. 
#' Thus, this routine knows when its values are being used to compute 
#' the likelihood and when its value is being used to compute the 
#' constant of integration.  All user defined likelihoods must have 
#' and use this parameter.
#' 
#' @param pointSurvey Boolean. TRUE if distances in \code{dist} are 
#' radial from point 
#' transects, FALSE if distances are perpendicular off-transect distances.
#' 
#' @details The half-normal likelihood is 
#' \deqn{f(x|a) = \exp(-x^2 / (2*a^2))}{f(x|a) = exp(-x^2 / (2*a^2))}
#' where \eqn{a} is the parameter to be estimated.
#' Some half-normal distance functions in the literature 
#' do not use a "2" in the 
#' denominator of the exponent.  \code{Rdistance} uses a 
#' "2" in the denominator of the exponent to make quantiles of this 
#' function agree with 
#' the standard normal which means \emph{a} can be interpreted as a 
#' normal standard error.  e.g., approximately 95\% of all observations 
#' will occur between 0 and 2\emph{a}.
#' 
#' \bold{Expansion Terms}: If \code{expansions} = k (k > 0), the expansion function specified by \code{series} is called (see for example
#'   \code{\link{cosine.expansion}}). Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} expansion term for the \eqn{i^{th}}{i-th} distance and that 
#'   \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)}are (estimated) coefficients for the expansion terms, the likelihood contribution for the \eqn{i^{th}}{i-th} 
#'   distance is, \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}
#'   {f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A numeric vector the same length and order as \code{dist} containing the 
#' likelihood contribution for corresponding distances in \code{dist}. 
#' Assuming \code{L} is the returned vector from one of these functions, 
#' the negative log likelihood of all the data is \code{-sum(log(L), na.rm=T)}. 
#' Note that the returned likelihood value for distances less 
#' than \code{w.lo} or greater than \code{w.hi} is \code{NA}, 
#' hence \code{na.rm=TRUE} in the sum. 
#' If \code{scale} = TRUE, the integral of the likelihood from
#'  \code{w.lo} to \code{w.hi} is 1.0. If \code{scale} = FALSE, 
#'  the integral of the likelihood is something else.
#'  
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#'         
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples  \dontrun{
#' set.seed(238642)
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in parameter Sigma
#' plot(x, halfnorm.like(20, x), type="l", col="red")
#' plot(x, halfnorm.like(40, x), type="l", col="blue")
#' 
#' # Estimate 'halfnorm' distance function
#' a <- 5
#' x <- rnorm(1000, mean=0, sd=a)
#' x <- x[x >= 0]
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc)
#' 
#' # evaluate the log Likelihood
#' L <- halfnorm.like(dfunc$parameters, dfunc$dist, covars=dfunc$covars, 
#'     w.lo=dfunc$w.lo, w.hi=dfunc$w.hi, 
#'     series=dfunc$series, expansions=dfunc$expansions, 
#'     scale=TRUE)
#' -sum(log(L), na.rm=TRUE)  # the negative log likelihood
#' }
#' @keywords models
#' @export

halfnorm.like <- function(a, 
                          dist, 
                          covars = NULL, 
                          w.lo = 0, 
                          w.hi = max(dist), 
                          series = "cosine", 
                          expansions = 0, 
                          scale = TRUE, 
                          pointSurvey = FALSE){

  # rule is: parameter 'a' never has units.  None of its components do, even though they could (e.g., sigma = a[1])
  # upon entry: 'dist', 'w.lo', and 'w.hi' all have units 
  dist[ (dist < w.lo) | (dist > w.hi) ] <- NA
  
  if(!is.null(covars)){
    
    q <- ncol(covars)
    # not necessary, in all half norm cases, no extra params hanging off the end 
    # but, I'll leave it here so it's compatible with other likelihoods and 
    # just in case we want to allow expansions with covariates later.
    beta <- a[1:q] 
    s <- drop( covars %*% matrix(beta,ncol=1) )
    sigma <- exp(s)
  } else {
    sigma <- a[1]
  }
  # cat(paste0("a[", 1:length(a), "]= ", a, "\n"))
  
  key <- -(units::drop_units(dist*dist))/(2*sigma*sigma)  
  # Above is safe. Units of sigma will scale to units of dist. 'key' is unit-less
  key <- exp(key)
  
  # If there are expansion terms
  if(expansions > 0){
    
    nexp <- expansions
    w <- w.hi - w.lo  # 'w' has units here, we want this so conversions below happen
    
    if (series=="cosine"){
      dscl <- units::drop_units(dist/w)   # unit conversion here; drop units is safe
      exp.term <- cosine.expansion( dscl, nexp )
    } else if (series=="hermite"){
      dscl <- units::drop_units(dist/sigma) # unit conversion here; drop units is safe
      exp.term <- hermite.expansion( dscl, nexp )
    } else if (series == "simple") {
      dscl <- units::drop_units(dist/w)    # unit conversion here; drop units is safe
      exp.term <- simple.expansion( dscl, nexp )
    } else {
      stop( paste( "Unknown expansion series", series ))
    }
    
    key <- key * (1 + c(exp.term %*% a[(length(a)-(nexp-1)):(length(a))]))
  } 
  
  if( scale ){
    key = key / integration.constant(dist=dist, 
                                         density=halfnorm.like, 
                                         a=a,
                                         covars = covars, 
                                         w.lo=w.lo, 
                                         w.hi=w.hi, 
                                         series=series, 
                                         expansions=expansions, 
                                         pointSurvey = pointSurvey)   # scales underlying density to integrate to 1.0

  }

  c(key)
}
