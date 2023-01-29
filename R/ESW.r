#' @title Line transect Effective Strip Width (ESW) 
#'   
#' @description Returns effective strip width (ESW) from an estimated 
#'   line transect detection
#'   functions. This function applies only to line transect information.
#'   Function \code{EDR} is for point transect data. Function 
#'   \code{effectiveDistance} accepts either point or line transect data. 
#'   
#' @param obj An estimated detection function object.  An estimated detection 
#'   function object has class 'dfunc', and is usually produced by a call to 
#'   \code{dfuncEstim}. The estimated detection function may optionally contain 
#'   a \eqn{g(0)} component that specifies detection probability 
#'   on the transect.  If no \eqn{g(0)} component is found, \eqn{g(0)} =
#'   1 is assumed.
#'   
#' @param newdata A data frame containing new values of 
#' the covariates at which ESW's are sought. If NULL or missing and 
#' \code{obj} contains covariates, the  
#' covariates stored in \code{obj} are used. See \bold{Value} section.
#'   
#' @details Effective strip width (ESW) of a distance function is its
#'   integral. That is, ESW is the area under the distance function from its
#'   left-truncation limit (\code{obj$w.lo}) to its right-truncation limit
#'   (\code{obj$w.hi}). \if{latex}{In mathematical notation, \deqn{ESW =
#'   \int_{w.lo}^{w.hi} g(x)dx,} where \eqn{g(x)} is the height of the distance
#'   function at distance \eqn{x}, and \eqn{w.lo} and \eqn{w.hi} are the lower
#'   and upper truncation limits used during the survey.  }
#'   
#'   If detection does not decline with distance, area under the detection 
#'   function is the entire half-width of 
#'   the strip transect (i.e., \code{obj$w.hi - obj$w.lo}).  
#'   In this case density is the number sighted targets 
#'   divided by area surveyed, where area surveyed is 
#'   \code{obj$w.hi-obj$w.lo} times
#'   total length of transects.
#'   
#'   When detection declines with distance, less than the total half-width is
#'   \emph{effectively} covered. In this case, Buckland \emph{et al.} (1993) show that the
#'   denominator of the density estimator is total length of 
#'   surveyed transects times area under the detection function (i.e., this
#'   integral). By analogy with the non-declining detection case, ESW is the 
#'   transect half-width that observers \emph{effectively}
#'   cover. In other words, if ESW = X, the study 
#'   effectively covers the same area as a study with non-declining detection out to a
#'   distance of X.
#'   
#'   \emph{A technical consideration}: Rdistance uses the trapezoid rule to numerically 
#'   integrate under the distance
#'   function from \code{obj$w.lo} to \code{obj$w.hi}. Two-hundred
#'   trapezoids are used in the approximation to speed calculations.  In some
#'   rare cases, two hundred trapezoids may not be enough.  In these cases, 
#'   users should modify this function's code and bump \code{seq.length} to 
#'   a value greater than 200.
#'   
#' @return If \code{newdata} is not missing and not NULL and 
#' covariates are present in \code{obj}, the returned value is 
#' a vector of ESW values associated with covariates in the 
#' distance function and equal in length to the number of rows in \code{newdata}. 
#' If \code{newdata} is missing or NULL and covariates are present
#' in \code{obj}, an ESW vector with length equal to 
#' the number of detections in \code{obj$detections} is returned. 
#' 
#' If \code{obj} does not contain covariates, \code{newdata} is ignored and 
#' a scalar equal to the (constant) effective strip width for all 
#' detections is returned.  
#'   
#' @references Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L.
#'   1993. \emph{Distance Sampling: Estimating Abundance of Biological
#'   Populations}. Chapman and Hall, London.
#'   
#' @author Trent McDonald
#' @seealso \code{\link{dfuncEstim}}, \code{\link{EDR}}, 
#' \code{\link{effectiveDistance}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData,
#'                     likelihood="halfnorm", w.hi=100, pointSurvey=FALSE)
#' 
#' # Compute effective strip width (ESW)
#' ESW(dfunc)
#' 
#' @keywords modeling
#' @export

ESW <- function( obj, newdata ){
  
  # Issue error if the input detection function was fit to point-transect data

  if(obj$pointSurvey) stop("ESW is for line transects only.  See EDR for the point-transect equivalent.")

  #   
  # like <- match.fun(paste( obj$like.form, ".like", sep=""))
  # 
  # 
  # # figure out scaling 
  # if( is.null( obj$g.x.scl ) ){
  #   #   Assume g0 = 1
  #   g.at.x0 <- 1
  #   x0 <- 0
  #   warning("g0 unspecified.  Assumed 1.")
  # } else {
  #   g.at.x0 <- obj$g.x.scl
  #   x0 <- obj$x.scl
  # }
  # 
  # zero <- units::set_units(x = 0
  #                          , value = obj$outputUnits
  #                          , mode = "standard")
  # 
  # if(  !is.null(obj$covars) ){
  #   # covariate case; eventually will return a vector
  #   if(missing(newdata)){
  #     newdata <- NULL  # in this case, predict.dfunc will use obj$covars, but gotta have something to pass
  #   }
  #   params <- predict(obj, newdata, type="parameters")
  # } else {
  #   # no covariates; eventually will return a scaler
  #   params <- matrix( obj$parameters, nrow = 1)
  # }
  # 
  # y <- apply(X = params
  #            , MARGIN = 1
  #            , FUN = like
  #            , dist = x - obj$w.lo
  #            , series = obj$series
  #            , covars = NULL
  #            , expansions = obj$expansions
  #            , w.lo = zero
  #            , w.hi = obj$w.hi - obj$w.lo
  #            , pointSurvey = FALSE
  #            , scale = FALSE
  #            )    
  # y <- t(y)
  # 
  # # at this point, y is either 1 X length(x) (no covars) or n X length(x) (covars).
  # # Each row is a unscaled distance function (does not integrate to one b.c., scale = F).
  # 
  # f.at.x0 <- apply(X = params
  #                  , MARGIN = 1
  #                  , FUN = like
  #                  , dist= x0 - obj$w.lo
  #                  , series = obj$series
  #                  , covars = NULL
  #                  , expansions=obj$expansions
  #                  , w.lo = zero
  #                  , w.hi=obj$w.hi - obj$w.lo
  #                  , pointSurvey = FALSE
  #                  , scale = FALSE
  # )
  #   
  # # If g.at.x0 = 1, we don't need to rescale b.c. scale = FALSE above; i.e. y[,1] = 1
  # # but, I will rescale even in this case, 
  # # just in case there are cases I have not thought about (e.g., 
  # # when like() does not have maximum at 1.0)
  # 
  # scaler <- g.at.x0 / f.at.x0 # a length n vector 
  # 
  # y <- y * scaler  # length(scalar) == nrow(y), so this works right

  seq.length = 200
  
  x.seq <- seq( obj$w.lo, obj$w.hi, length=seq.length)
  dx <- x.seq[3] - x.seq[2]
  
  y <- predict.dfunc(object = obj
                     , newdata = newdata
                     , distance = x.seq
                     , type = "distances"
  )
  
  # Eventually, will get all the numerical integration 
  # into one routine (or use R built-in integrate())
  #
  # Trapazoid rule: Computation used in Rdistance version < 0.2.2
  # y1 <- y[,-1,drop=FALSE]
  # y  <- y[,-ncol(y),drop=FALSE]
  # esw <- dx*rowSums(y + y1)/2
  
  # Trapezoid rule. (dx/2)*(f(x1) + 2f(x_2) + ... + 2f(x_n-1) + f(n)) 
  # Faster than above, maybe.
  ends <- c(1,nrow(y))
  esw <- (dx/2) * (colSums( y[ends, ,drop=FALSE] ) + 
                   2*colSums(y[-ends, ,drop=FALSE] ))

  esw
  
}
