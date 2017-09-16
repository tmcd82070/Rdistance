#' @title Effective Strip Width for line transect data
#'   
#' @description Computes effective strip width (ESW) for estimated detection
#'   functions from line transect data
#'   
#' @param obj An estimated detection function object.  An estimated detection 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component.  If no \eqn{g(0)} component is found, \eqn{g(0)} =
#'   1 is assumed.
#'   
#' @param newdata A data frame containing new values of 
#' the covariates at which ESW's are sought. If NULL or missing, the 
#' covariates stored in \code{obj} are used. If covariates are present, 
#' this produces one ESW for each detection.  If covariates are not present, 
#' this produces one ESW that is constant across all detections.
#'   
#' @details The effective strip width (ESW) of a distance function is its
#'   integral. That is, ESW is the area under the distance function from its
#'   left-truncation limit (\code{obj$w.lo}) to its right-truncation limit
#'   (\code{obj$w.hi}). \if{latex}{In mathematical notation, \deqn{ESW =
#'   \int_{w.lo}^{w.hi} g(x)dx,} where \eqn{g(x)} is the height of the distance
#'   function at distance \eqn{x}, and \eqn{w.lo} and \eqn{w.hi} are the lower
#'   and upper truncation limits used during the survey.  }
#'   
#'   The name, \emph{effective strip width}, comes from the fact that under
#'   perfect detection, area under the detection function is the entire 
#'   half-width of 
#'   the strip transect (from \code{obj$w.lo} to \code{obj$w.hi}).  
#'   Under perfect detection, density is the number sighted targets 
#'   divided by area surveyed, where area surveyed is 
#'   \code{obj$w.hi-obj$w.lo} times
#'   total length of transects.
#'   
#'   When detection is not perfect, less than the total half-width is
#'   \emph{effectively} covered. Buckland et al. (1993) show that the
#'   denominator of the density estimator in this case involves total length of 
#'   transects surveyed times area under the detection function (i.e., this
#'   integral). By analogy with the perfect detection case, this integral can
#'   then be viewed as the transect half-width that observers \emph{effectively}
#'   cover. In other words, a survey with imperfect detection and ESW equal to X
#'   effectively covers the same area as a study with perfect detection out to a
#'   distance of X.
#'   
#'   The trapazoid rule is used to numerically integrate under the distance
#'   function in \code{obj} from \code{obj$w.lo} to \code{obj$w.hi}. Two-hundred
#'   trapazoids are used in the approximation to speed calculations.  In some
#'   rare cases, two hundred trapazoids may not be enough.  In these cases, the
#'   code for this function can be \code{sink}-ed to a file, inspected in a text
#'   editor, modified to bump the number of trapazoids, and \code{source}-d back
#'   in.
#' @return If covariates are present in \code{obj}, a vector of 
#' length equal to the number of detections in \code{obj$dist} containing
#' separate ESW's for each detection. If 
#' \code{obj} does not contain covariates, a scalar 
#' equal to the (constant) ESW for all detections.  In all cases, ESW
#' is the area under the detection function from
#'   \code{obj$w.lo} to \code{obj$w.hi}.
#'   
#' @references Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L.
#'   1993. \emph{Distance Sampling: Estimating Abundance of Biological
#'   Populations}. Chapman and Hall, London.
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{dfuncEstim}}, \code{\link{EDR}}
#' @examples # Load the example dataset of sparrow detections from package
#' data(sparrowDetectionData)
#' 
#' # Fit detection function to perpendicular, off-transect distances
#' dfunc <- dfuncEstim(sparrow.detections, w.hi=150)
#' 
#' # Compute effective strip width (ESW)
#' ESW(dfunc)
#' @keywords modeling
#' @export

ESW <- function( obj, newdata ){
  
  # Issue error if the input detection function was fit to point-transect data

  if(obj$pointSurvey) stop("ESW is for line transects only.  See EDR for the point-transect equivalent.")
  
  like <- match.fun(paste( obj$like.form, ".like", sep=""))
  
  seq.length = 200

  # Can't evaluate hazrate at 0
  if( (obj$like.form == "hazrate") & (obj$x.scl == obj$w.lo) ){
    x <- seq( obj$w.lo + 1e-6*(obj$w.hi - obj$w.lo), obj$w.hi, length=seq.length)
  } else {
    x <- seq( obj$w.lo, obj$w.hi, length=seq.length)
  }

  # figure out scaling 
  if( is.null( obj$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- 0
    warning("g0 unspecified.  Assumed 1.")
  } else {
    g.at.x0 <- obj$g.x.scl
    x0 <- obj$x.scl
  }
  
  if( is.null(obj$covars) ){
    # no covariates case; return scalar
    y <- like( obj$parameters, x - obj$w.lo, series=obj$series, covars = NULL, 
               expansions=obj$expansions, w.lo = obj$w.lo, w.hi=obj$w.hi, 
               pointSurvey = obj$pointSurvey )


    f.at.x0 <- like( obj$parameters, x0 - obj$w.lo, series=obj$series, 
                     covars = NULL, expansions=obj$expansions, 
                     w.lo=obj$w.lo, w.hi=obj$w.hi, 
                     pointSurvey = obj$pointSurvey )
    
    y <- y * g.at.x0 / f.at.x0
    
    # Trapazoid rule.  
    # [tlm] not sure I agree with this old comment: Use x[3] and x[2] because for 
    # hazard rate, x[1] is not evenly spaced with rest.
        
    esw <- (x[3] - x[2]) * sum(y[-length(y)]+y[-1]) / 2 
    
  } else {
    # covariate case; return vector
    if(missing(newdata)){
      newdata <- NULL  # in this case, predict.dfunc will use obj$covars, but gotta have something to pass
    }

    params <- predict.dfunc(obj, newdata, type="parameters")

    # Use covars= NULL here because we evaluated covariates to get params above
    # after this, y is n X length(x).  each row is a unscaled distance 
    # function (f(x))
    y <- apply(params, 1, like, dist= x - obj$w.lo, 
               series=obj$series, covars = NULL, 
               expansions=obj$expansions, 
               w.lo = obj$w.lo, w.hi=obj$w.hi, 
               pointSurvey = obj$pointSurvey )    
    y <- t(y)

    f.at.x0 <- apply(params, 1, like, dist= x0 - obj$w.lo, 
                     series=obj$series, covars = NULL, 
                     expansions=obj$expansions, 
                     w.lo=obj$w.lo, w.hi=obj$w.hi, 
                     pointSurvey = obj$pointSurvey )
    scaler <- g.at.x0 / f.at.x0 # a length n vector 
    
    y <- y * scaler  # length(scalar) == nrow(y), so this works right

    # Trapazoid rule.  
    dy <- x[3]-x[2]
    y1 <- y[,-1]
    y  <- y[,-ncol(y)]
    esw <- dy*rowSums(y + y1)/2
  }
  
  esw
  
}
