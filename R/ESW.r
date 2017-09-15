#' @title Effective Strip Width for estimated detection functions
#' 
#' @description Computes effective strip width (ESW) for 
#' estimated detection functions.
#' 
#' @param obj An estimated detection function object.  An estimated detection
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{F.dfunc.estim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} = 1
#'   is assumed.
#'   
#' @param covars Covariate values for which to calculate ESW.
#' 
#' @details The effective strip width (ESW) of a distance function is its integral. 
#'   That is, ESW is the area under the 
#'   distance function from its left-truncation limit (\code{obj$w.lo}) to its 
#'   right-truncation limit (\code{obj$w.hi}). 
#'   \if{latex}{In mathematical notation,
#'     \deqn{ESW = \int_{w.lo}^{w.hi} g(x)dx,}
#'     where \eqn{g(x)} is the height of the distance function at distance \eqn{x}, 
#'     and \eqn{w.lo} and \eqn{w.hi}
#'     are the lower and upper truncation limits used during the survey.  }
#' 
#'   The name \emph{effective strip width} derives from the fact that under perfect detection, 
#'   area under the detection function is the half-width of the strip transect.  This means
#'   that if \code{obj$w.lo} = 0 and \eqn{g(x)} = 1,
#'   area under the detection function is the half-width of the transect (i.e., \code{obj$w.hi}). 
#'   In this case, the density of objects is estimated as number sighted 
#'   divided by area surveyed, which is \code{obj$w.hi} times total length of transects surveyed. 
#' 
#'   When detection is not perfect, less than the total half-width is \emph{effectively} covered.  
#'   Buckland et al. (1993)
#'   show that the denominator of the density estimator in this case involves total length of 
#'   transects surveyed times area under the detection function (i.e., this integral). By analogy with the 
#'   perfect detection case, this integral can then be viewed as the 
#'   transect half-width that observers \emph{effectively} cover. In other words, a survey with imperfect detection
#'   and ESW equal to X effectively covers the same area as a study with perfect detection out to a distance of X.
#' 
#'   The trapazoid rule is used to numerically integrate under the distance function 
#'   in \code{obj} from \code{obj$w.lo} to \code{obj$w.hi}. Two-hundred trapazoids are 
#'   used in the approximation to speed calculations.  In some rare cases, two hundred trapazoids 
#'   may not be enough.  In these cases, the code for this function can be \code{sink}-ed to a file, 
#'   inspected in a text editor, modified 
#'   to bump the number of trapazoids, and \code{source}-d back in.
#' @return A scalar equal to the area under the detection function from \code{obj$w.lo} to \code{obj$w.hi}.
#' @references Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L. 1993. \emph{Distance Sampling: Estimating Abundance of Biological Populations}. Chapman and Hall, London.
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{F.dfunc.estim}}
#' @examples # Load the example dataset of sparrow detections from package
#' data(sparrow.detections)
#' 
#' # Fit detection function to perpendicular, off-transect distances
#' dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)
#' 
#' # Compute effective strip width (ESW)
#' ESW(dfunc)
#' @keywords modeling
#' @export

ESW <- function( obj, covars = NULL ){
  
  # Issue error if the input detection function was fit to point-transect data
  # Eventually, it would be nice to build the effective.radius code into ESW, so it could handle either.
  if(obj$point.transects) stop("ESW is for line transects only.  See effective.radius for the point-transect equivalent.")
  
  
#
#   obj = a dfunc object.  It may optionally contain a g0 component.
#       if no g0, assume g0 = 1
#
#   Note, For the classic distance functions, the intergral from 0 to w of g(x) = ESW 
#   is the ratio g(x) / f(x) for any x.  If we know g(0), we can compute f(0), and 
#   take the ratio g(0) / f(0).  However, in general and for other distance functions
#   we will do numerical integration.  This is a bit slower, and a bit less accurate in 
#   some cases, but is more general.  This allows user defined distance functions to be 
#   added easily. 
if(is.null(covars)){
  covars <- obj$covars
}
  
like <- match.fun(paste( obj$like.form, ".like", sep=""))

if( is.null(obj$covars) )
  seq.length = 200
else
  seq.length = length(obj$covars*10)

if( (obj$like.form == "hazrate") & (obj$x.scl == obj$w.lo) ){
    x <- seq( obj$w.lo + 1e-6*(obj$w.hi - obj$w.lo), obj$w.hi, length=seq.length)
} else {
    x <- seq( obj$w.lo, obj$w.hi, length=seq.length)
}

y <- like( obj$parameters, x - obj$w.lo, series=obj$series, covars = covars, expansions=obj$expansions, w.lo = obj$w.lo, w.hi=obj$w.hi, point.transects = obj$point.transects )


if( is.null( obj$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- 0
    warning("g0 unspecified.  Assumed 1.")
} else {
    g.at.x0 <- obj$g.x.scl
    x0 <- obj$x.scl
}
f.at.x0 <- like( obj$parameters, x0 - obj$w.lo, series=obj$series, covars = covars, expansions=obj$expansions, w.lo=obj$w.lo, w.hi=obj$w.hi, point.transects = obj$point.transects )

y <- y * g.at.x0 / f.at.x0

  esw <- (x[3] - x[2]) * sum(y[-length(y)]+y[-1]) / 2   # Trapazoid rule.  Use x[3] and x[2] because for hazard rate, x[1] is not evenly spaced with rest

### This is correct. It works provided g(x) and x are stored in obj. 
### This routine evaluates f(x) using like(), 
### and then takes ratio of g(x) / f(x). 
#esw2 <- g.at.x0 / f.at.x0

#print( c(esw2, esw))

esw

}
