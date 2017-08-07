#' @name F.nLL
#' @aliases F.nLL
#' @title F.nLL - Return the negative log likelihood for a set of distance values.
#' @description Return value of the negative log likelihood for a set of observed off-transect distances given a specified likelihood, 
#'   number of expansion terms, and estimated parameters.
#' @usage F.nLL(a, dist, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0, point.transects, for.optim = F)
#' @param a A vector of parameter values to use to compute the liklihood. Length of this vector must be 
#'   \code{expansions + 1 + 1*(like \%in\% c("hazrate", "uniform"))}.
#' @param dist A vector of observed off-transect distances. Must be between \code{w.lo} and \code{w.hi} (see below).
#' @param covars Data frame containing values of covariates at each observation in \code{dist}.
#' @param like String specifying the form of the likelihood.  Valid values at present are "uniform", "halfnorm", "hazrate", "negexp", and "Gamma".
#' @param w.lo Lower or left-truncation limit of the distances.  This is the minimum possible off-transect distance. Default is 0.
#' @param w.hi Upper or right-truncation limit of the distances. This is the maximum off-transect distance that could be observed. 
#'   Default is the maximum observed distance.
#' @param series String specifying the type of expansion to use series if \code{expansions} > 0. Valid values at present are 'simple', 'hermite',
#'   and 'cosine'.
#' @param expansions A scalar specifying the number of terms in \code{series} to compute. Depending on the series, this could be 0 through 5. 
#'   The default of 0 equates to no expansion terms of any type.
#' @param point.transects Boolean. TRUE if \code{dist} is point transect data, FALSE if line transect data.
#' @param for.optim Boolean. If TRUE, values are multpilied by 10^9 to help optim converge more consistently.
#' @return A scalar, the negative of the log likelihood given the parameters in \code{a}, likelihood, and expansion terms.
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#' @seealso See \code{\link{uniform.like}} for definitions of all the likelihoods; \code{\link{F.dfunc.estim}}
#' @keywords models
#' @export

F.nLL <- function(a, dist, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0, point.transects, for.optim = F){

    f.like <- match.fun(paste( like, ".like", sep=""))
   
    L <- f.like( a = a, dist = dist, covars = covars, w.lo = w.lo, w.hi = w.hi, series = series, expansions = expansions, point.transects = point.transects)
    
    if(for.optim){
      L <- L*10^9
    }
    
    #print(L)
    L[ !is.na(L) & (L <= 0) ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(L), na.rm=TRUE)  # Note that distances > w in LL are set to NA
    #print(nLL)
    nLL
}
