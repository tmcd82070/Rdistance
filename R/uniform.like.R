#' @name uniform.like
#' @title Uniform likelihood function for distance analyses.
#' @description This function computes likelihood contributions for off-transect sighting distances, scaled appropriately, for use as a distance likelihood.
#' @usage uniform.like(a, dist, covars = NULL, w.lo=0, w.hi=max(dist), series="cosine", expansions=0, scale=TRUE, point.transects = F, ...)
#' @param a A vector of likelihood parameter values. Length and meaning depend on \code{series} and \code{expansions}. If no expansion terms were called for
#'   (i.e., \code{expansions = 0}), the distance likelihoods contain one or two canonical parameters (see Details). If one or more expansions are called for,
#'   coefficients for the expansion terms follow coefficients for the canonical parameters.  If \code{p} is the number of canonical parameters, coefficients
#'   for the expansion terms are \code{a[(p+1):length(a)]}.
#' @param dist A numeric vector containing the observed distances.
#' @param covars Data frame containing values of covariates at each observation in \code{dist}.
#' @param w.lo Scalar value of the lowest observable distance.  This is the \emph{left truncation} of sighting distances in \code{dist}. Same units as \code{dist}.
#'   Values less than \code{w.lo} are allowed in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output.
#' @param w.hi Scalar value of the largest observable distance.  This is the \emph{right truncation} of sighting distances in \code{dist}.  Same units as \code{dist}.
#'   Values greater than \code{w.hi} are allowed in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output.
#' @param series A string specifying the type of expansion to use.  Currently, valid values are 'simple', 'hermite', and 'cosine'; but, see 
#'   \code{\link{F.dfunc.estim}} about defining other series.
#' @param expansions A scalar specifying the number of terms in \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#' @param scale Logical scaler indicating whether or not to scale the likelihood so it integrates to 1. This parameter is used to stop recursion in other functions.
#'   If \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) is called, which in turn calls this likelihood function again
#'   with \code{scale} = FALSE. Thus, this routine knows when its values are being used to compute the likelihood and when its value is being used to compute the 
#'   constant of integration.  All user defined likelihoods must have and use this parameter.
#' @param point.transects Boolean. TRUE if \code{dist} is point transect data, FALSE if line transect data.
#' @details The uniform likelihood is not technically uniform, but can look similar to a uniform if the data warrant.  The uniform likelihood is actually the 
#'   \emph{heavy side} or \emph{logistic} function of the form, \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}
#'   {f(x|a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-a))),} where \eqn{a} and \eqn{b} are the parameters to be estimated.  Parameter 
#'   \eqn{a} can be thought of as the location of the "approximate upper limit" of an uniform distribution's support by noting that that the inverse likelihood of 
#'   0.5 is \code{a} before scaling (i.e., \code{uniform.like(c(a,b),a,scale=FALSE)} equals \code{0.5}). Parameter \code{b} is the "knee" or sharpness of the bend
#'   at \code{a} and estimates the degree to which observations decline at the outer limit of sightability.  Note that, prior to scaling for \code{g.x.scl}, 
#'   the slope of the likelihood at \eqn{a} is \eqn{-b/4}. After scaling for \code{g.x.scl}, the inverse of \code{g.x.scl/2} is close to \code{a/f(0)}. If \eqn{b}
#'   is large, the "knee" is sharp and the density of observations  declines rapidly at \eqn{a/f(0)}. In this case, the likelihood can look like a uniform 
#'   distribution with support from \code{w.lo} to \eqn{a/f(0)}.  If \eqn{b} is small, the "knee" is shallow and the density of observations declines in an elongated 
#'   "S" shape pivoting at \code{a/f(0)}.  As  \code{b} grows large and assuming f(0) = 1, the effective strip width approaches \code{a} from above.  
#'   See Examples for plots using large and small values of \eqn{b}. 
#'   \bold{Expansion Terms}: If \code{expansions} = k (k > 0), the expansion function specified by \code{series} is called (see for example
#'   \code{\link{cosine.expansion}}). Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} expansion term for the \eqn{i^{th}}{i-th} distance and that 
#'   \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)}are (estimated) coefficients for the expansion terms, the likelihood contribution for the \eqn{i^{th}}{i-th} 
#'   distance is, \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}
#'   {f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#' @return A numeric vector the same length and order as \code{dist} containing the likelihood contribution for corresponding distances in \code{dist}. 
#'   Assuming \code{L} is the returned vector from one of these functions, the full log likelihood of all the data is \code{-sum(log(L), na.rm=T)}. Note that the
#'   returned likelihood value for distances less than \code{w.lo} or greater than \code{w.hi} is \code{NA}, and thus it is prudent to use \code{na.rm=TRUE} in the
#'   sum. If \code{scale} = TRUE, the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0. If \code{scale} = FALSE, the integral of the likelihood is
#'   arbitrary.
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#' @keywords models
#' @export

uniform.like <- function(a, dist, covars = NULL, w.lo = 0, w.hi = max(dist), series = "cosine", expansions = 0, scale = TRUE, point.transects = F, ...){
#
#   Compute the uniform likelihood, scaled appropriately, for all distance values in dist.
#
#   The likelihood is actually a "heavy-side" function of the form,
#
#       f(x; a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp( -b*(x-a) ) / (1 + exp( -b*(x-a) ))
#
#   which you can see is basically a logistic function.  Parameter a is the location of the
#   "discontinuity" (=inversef(.5) = a) and b is the "knee" or sharpness of the knee at a.
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions. If
#       no expansion terms called for (ie., straight logistic) a is
#       length 2, where a[1] is upper end of support (i.e., density is approximately U[0,a]),
#       and a[2] is sharpness of the decline (or knee) at a[1]
#       If expansions are called for a[3:length(a)] are coefficients of the
#       expansion.
#   dist = input observed distance data
#   w = right truncation value, same units as dist
#   series = character values specifying type of expansion.  Currently only
#       "cosine" and "simple" work. Default is no series expansions.
#   expansions = number of expansion terms.  This controls whether series
#       expansion terms are fitted. Default = 0 does not fit any terms.
#       If >0, fit the number of expansion terms specified of the type
#       specified by series.  Max terms depends on series.
#   scale = logical, whether or not to scale the likelihood values so
#       that the underlying density integrates to 1. This parameter is used
#       to stop the recursion.
#
#   Output:
#   A vector same length as dist, containing likelihood values, scaled appropriately.
#   Likelihood for all distances >w are set to NA
#	


    #   A couple internal functions first.
    #   This is the heavy-side function.  Basically, a steep logistic. f is just heavi flipped over
    heavi <- function(x,k){ 1 / (1 + exp( -k*x ))}
    f <- function(beta1, beta2, x){ 1 - heavi(x-beta1,beta2) }


    dist[ (dist < w.lo) | (dist > w.hi) ] = NA
    beta <- c(0,0)
    if(!is.null(covars)){
      s <- 0
      for (i in 1:(ncol(covars)))
        s <- s + a[i]*covars[,i]
      beta1 <- exp(s)
    } else {beta1 <- a[1]}
    beta2 <- a[length(a)-expansions]
    
	key <- f(beta1, beta2, dist)
    dfunc <- key
    w <- w.hi - w.lo
#    cat(paste( "w.lo=", w.lo, "w.hi=", w.hi, "\n"))

    # If there are expansion terms
    if(expansions > 0){

        nexp <- expansions #min(expansions,length(a)-2)  # should be equal. If not, fire warning next

        #if( length(a) != (expansions+2) ) {
            #warning("Wrong number of parameters in expansion. Should be (expansions+2). High terms ignored.")
        #}

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/ (a[1]/sqrt(12))  # denom is approx std of U[0,a[1]]
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + c(exp.term %*% a[(length(a)-(nexp-1)):(length(a))]))
    }

    if( scale ){
            dfunc = dfunc / integration.constant(dist, uniform.like, covars = covars, w.lo=w.lo, w.hi=w.hi, a=a,series=series, expansions=expansions, point.transects = point.transects, ...)   # scales density so integrate from w.lo to w.hi is 1.0
    }

#   df2 <- dfunc[ order(dist) ]
#   d2 <- dist[ order(dist) ]
#   cat(paste("integral=", sum( diff(d2) * (df2[-1] + df2[-length(df2)]) ) / 2, "\n" ))
#   readline("Enter:")

    c(dfunc)
}
