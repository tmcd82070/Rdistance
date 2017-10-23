#' @name nexexp.like
#' @title Negative exponential likelihood function for distance analyses
#' @description This function computes likelihood contributions for off-transect sighting distances, scaled appropriately, for use as a distance likelihood.
#' @usage negexp.like(a, dist, covars = NULL, w.lo=0, w.hi=max(dist), series="cosine", expansions=0, scale=TRUE, pointSurvey = F, ...)
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
#'   \code{\link{dfuncEstim}} about defining other series.
#' @param expansions A scalar specifying the number of terms in \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#' @param scale Logical scaler indicating whether or not to scale the likelihood so it integrates to 1. This parameter is used to stop recursion in other functions.
#'   If \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) is called, which in turn calls this likelihood function again
#'   with \code{scale} = FALSE. Thus, this routine knows when its values are being used to compute the likelihood and when its value is being used to compute the 
#'   constant of integration.  All user defined likelihoods must have and use this parameter.
#' @param pointSurvey Boolean. TRUE if \code{dist} is point transect data, FALSE if line transect data.
#' @details The negative exponential likelihood is \deqn{f(x|a) = \exp(-ax)}{f(x|a) = exp( -a*x )} where \eqn{a} is a slope parameter to be estimated. 
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
#' @author Trent McDonald, WEST Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples \dontrun{
#' set.seed(238642)
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in parameter Beta
#' plot(x, negexp.like(0.01, x), type="l", col="red")
#' plot(x, negexp.like(0.05, x), type="l", col="blue")
#' 
#' # Estimate 'negexp' distance function
#' Beta <- 0.01
#' x <- rexp(1000, rate=Beta)
#' dfunc <- dfuncEstim(x~1, likelihood="negexp")
#' plot(dfunc)
#' }
#'          
#' @keywords models
#' @export

negexp.like <- function (a, dist, covars = NULL, w.lo = 0, w.hi = max(dist),
                         series = "cosine", expansions = 0, scale = TRUE, pointSurvey = F, ...){
#
#   Compute negative exponential likelihood
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions.
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
#   Details: 	
#   Excerpted from DISTANCE online help manual:

#   The candidate key functions offered are Uniform, Half-normal, Hazard-rate and
#   Negative exponential (this last function is not recommended, except for salvage
#   analyses; see Buckland et al. 1993, 2001). The candidate series expansions are
#   Cosine, Simple polynomial and Hermite polynomial.
#
#   Bounds on Key Function Parameters
#   With some datasets, it may be necessary to bound the parameter estimates to
#   achieve convergence. In these situations, you can use this table to specify upper
#   and lower bounds on the key function parameters
#
#   (one parameter for half-normal and negative exponential key functions,
##--THIS IS WHERE I GOT THE ONE PARAMETER NEG EXP
#
#   two for the hazard rate function, plus any covariate parameters). One common
#   circumstance where this is required is to impose a lower bound of 1.0 on the
#   second hazard rate parameter

    dist[ (dist < w.lo) | (dist > w.hi) ] <- NA

    if(!is.null(covars)){
      s <- 0
      for (i in 1:(ncol(covars)))
        s <- s + a[i]*covars[,i]
      beta <- exp(s)
    } else {beta <- a[1]}
	key = exp(-beta*dist)
    dfunc <- key
    w <- w.hi - w.lo


    if(expansions > 0){

        nexp <- min(expansions,length(a)-1)  # should be equal. If not, fire warning next
        
        if( length(a) != (expansions+1) ) {
            #warning("Wrong number of parameters in expansion. Should be (expansions+1). High terms ignored.")
        }

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/w
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + c(exp.term %*% a[(length(a)-(nexp-1)):(length(a))]))


    } else if(length(a) > 1){
        #warning("Wrong number of parameters in halfnorm. Only 1 needed if no expansions. High terms ignored.")
    }

    if( scale ){
        dfunc = dfunc / integration.constant(dist, negexp.like, covars = covars, w.lo=w.lo,w.hi=w.hi,a=a,series=series,expansions=expansions, pointSurvey = pointSurvey, ...)  # makes integral from w.lo to w.hi = 1.0
    }
    
    c(dfunc)
}
