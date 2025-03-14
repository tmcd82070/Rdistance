#' @title predict.dfunc - Predict distance functions
#' 
#' @description Predict either likelihood parameters, 
#' distance functions, site-specific density, or 
#' site-specific abundance from estimated distance function 
#' objects.
#' 
#' @param object An Rdistance model frame or fitted distance function,
#' normally produced by a call to \code{\link{dfuncEstim}}. 
#' 
#' @param newdata A data frame containing new values of 
#' the covariates at which to evaluate the distance functions. 
#' If \code{newdata}
#' is NULL, distance functions are evaluated at values of 
#' the observed covariates and results in one prediction 
#' per distance or transect (see parameter \code{type}). 
#' If \code{newdata} is not NULL and the model does not contains covariates, 
#' this routine returns one prediction for each row in \code{newdata}, but 
#' columns and values in \code{newdata} are ignored. 
#' 
#' @param type The type of predictions desired. 
#' \itemize{
#'   \item \bold{If \code{type} == "parameters"}: Returned value is 
#'     a matrix of predicted (canonical) parameters of the 
#'     likelihood function. If \code{newdata} is NULL, return contains 
#'     one parameter value for every detection distance
#'     in \code{object$mf} (distances in \code{object$mf} are between 
#'     \code{object$w.lo} and \code{object$w.hi} and non-missing). 
#'     If \code{newdata} is not NULL, returned vector 
#'     has one parameter for every row in \code{newdata}. Parameter 
#'     \code{distances} is ignored when \code{type} == "parameters". 
#'     Canonical parameters (non-expansion terms)
#'     are returned on the response (inverse-link) scale.  Raw 
#'     canonical parameters in \code{object$par} are stored in 
#'     the link scale.  Expansion term parameters use the identity 
#'     link, so their value in the output equals their value 
#'     in \code{object$par}. 
#'     
#'   \item \bold{If \code{type} == "likelihood"}: Returned value is a 
#'     matrix of \bold{unscaled} likelihood values for all  
#'     observed distances in \code{object$mf}, i.e., raw distance functions
#'     evaluated at the observed distances.  Parameters \code{newdata} and 
#'     \code{distances} are ignored when \code{type} is "likelihood". 
#'     The negative log likelihood of the full data set is
#'     \code{-sum(log(predict(object,type="likelihood") / 
#'     effectiveDistance(object)))}. 
#'     
#'   \item \bold{If \code{type} == "dfuncs" or "dfunc"}: Returned  
#'     value is a matrix whose columns contain scaled distance functions. 
#'     The distance functions in each column are evaluated at distances   
#'     in argument \code{distances}, not at the observed 
#'     distances in \code{object$mf}. The number of distance functions
#'     returned (i.e., number of columns) depends on \code{newdata} 
#'     as follows: 
#'     \itemize{
#'        \item If \code{newdata} is NULL, one distance function 
#'        will be returned for every detection in \code{object$mf}
#'        that has valid covariate values.    
#'       \item If \code{newdata} is not NULL, one distance function 
#'       will be returned for each observation (row) in \code{newdata}. 
#'     }
#'     
#'    \item \bold{If \code{type} == "density" or "abundance"}: Returned 
#'    object is a tibble containing predicted density and abundance 
#'    on the area surveyed by each transect. 
#'  }  
#'  
#'  If \code{object} is a smoothed distance function, it does not have parameters
#'  and this routine will only return scaled distance functions, densities, or 
#'  abundances. That is, 
#'  \code{type} = "parameters" when \code{object} is smoothed 
#'  does not make sense and the smoothed distance function estimate 
#'  will be returned if \code{type} does not equal "density" or "abundance". 
#' 
#' @param distances A vector or 1-column matrix of 
#' distances at which to evaluate 
#' distance functions, when distance functions 
#' are requested.  \code{distances} must have measurement units. 
#' Any distances outside the observation 
#' strip (\code{object$w.lo} to \code{object$w.hi}) are discarded.  If 
#' \code{distances} is NULL, a sequence 
#' of \code{getOption("Rdistance_intEvalPts")} (default 101) evenly 
#' spaced distances between 
#' \code{object$w.lo} and \code{object$w.hi} (inclusive) is used. 
#' 
#' @inheritParams abundEstim
#'
#' @param \dots Included for compatibility with generic \code{predict} methods.
#' 
#' @return A matrix containing predictions: 
#' \itemize{
#'   \item \bold{If \code{type} is "parameters"}, the returned matrix 
#'   contains likelihood parameters. The extent of the 
#'   first dimension (rows) in the returned matrix is equal to 
#'   either the number of detection distances 
#'   in the observed strip or number of rows in \code{newdata}. 
#'   The returned matrix's second dimension (columns) is 
#'   the number of parameters in the likelihood 
#'   plus the number of expansion terms.  
#'   See the help for each likelihoods to interpret  
#'   returned parameter values. All parameters are returned 
#'   on the inverse-link scale; i.e., \emph{exponential} for canonical 
#'   parameters and \emph{identity} for expansion terms. 
#'   
#'   \item \bold{If \code{type} is "dfuncs" or "dfunc"}, columns of the 
#'   returned matrix contains detection functions (i.e., \emph{g(x)}).  
#'   The extent of the first 
#'   dimension (number of rows) is either the number of distances 
#'   specified in \code{distances}
#'   or \code{options()$Rdistance_intEvalPts} if \code{distances} is 
#'   not specified.
#'   The extent of the second dimension (number of columns) is: 
#'     \itemize{
#'       \item the number of detections with non-missing distances: 
#'       if \code{newdata} is NULL.
#'       \item the number of rows in \code{newdata} if 
#'        \code{newdata} is specified.
#'     }
#'   All distance functions in columns of the return are scaled 
#'   to \code{object$g.x.scale} at \code{object$x.scl}. The returned matrix has 
#'   the following additional attributes:
#'    \itemize{
#'       \item \code{attr(return, "distances")} is the vector of distances used to 
#'       predict the function in \code{return}.  Either the input \code{distances} object
#'       or the computed sequence of distances when \code{distances} is NULL. 
#'       \item \code{attr(return, "x0")} is the vector of distances at which each 
#'        distance function in \code{return} was scaled. i.e., the vector of 
#'        \code{x.scl}.
#'       \item \code{attr(return, "g.x.scl")} is the height of \emph{g(x)} (the distance 
#'        function) at \emph{x0}. 
#'   }
#'   
#'   \item \bold{If \code{type} is "density" or "abundance"}, the return is a 
#'   tibble containing density and abundance estimates by transect.  
#'   All transects in the input data (i.e., \code{object$data}) are 
#'   included, even those with missing lengths. 
#'    Columns in the tibble are:
#'    \itemize{
#'      \item transect ID: the grouping factor of the original RdistDf object.
#'      \item individualsSeen: sum of non-missing group sizes on that transect.
#'      \item avgPdetect: average probability of detection over groups sighted on that transect.
#'      \item effort: size of the area surveyed by that transect.
#'      \item density: density of individuals in the area surveyed by the transect.
#'      \item abundance: abundance of individuals in the area surveyed by the transect.
#'    } 
#' }
#' 
#' @seealso \code{\link{halfnorm.like}}, \code{\link{negexp.like}}, 
#' \code{\link{hazrate.like}}
#' 
#' @examples
#' 
#' data("sparrowDf")
#'
#' # For dimension checks:
#' nd <- getOption("Rdistance_intEvalPts")
#' 
#' # No covariates
#' dfuncObs <- sparrowDf |> dfuncEstim(formula = dist ~ 1
#'                      , w.hi = units::as_units(100, "m"))
#'                      
#' n  <- nrow(dfuncObs$mf)
#' p <- predict(dfuncObs) # parameters
#' all(dim(p) == c(n, 1)) 
#' 
#' # values in newdata ignored because no covariates
#' p <- predict(dfuncObs, newdata = data.frame(x = 1:5))
#' all(dim(p) == c(5, 1)) 
#' 
#' # Distance functions in columns, one per observation
#' p <- predict(dfuncObs, type = "dfunc") 
#' all(dim(p) == c(nd, n))
#' 
#' d <- units::set_units(c(0, 20, 40), "ft")
#' p <- predict(dfuncObs, distances = d, type = "dfunc") 
#' all(dim(p) == c(3, n))
#' 
#' p <- predict(dfuncObs
#'    , newdata = data.frame(x = 1:5)
#'    , distances = d
#'    , type = "dfunc") 
#' all(dim(p) == c(3, 5))
#' 
#' # Covariates
#' dfuncObs <- sparrowDf |> dfuncEstim(formula = dist ~ observer
#'                      , w.hi = units::as_units(100, "m"))
#' predict(dfuncObs)  # n X 1
#' 
#' Observers <- data.frame(observer = levels(sparrowDf$observer))
#' predict(dfuncObs, newdata = Observers) # 5 X 1
#' 
#' predict(dfuncObs, type = "dfunc") # nd X n
#' predict(dfuncObs, newdata = Observers, type = "dfunc") # nd X 5
#' predict(dfuncObs
#'   , newdata = Observers
#'   , distances = d
#'   , type = "dfunc") # 3 X 5
#' 
#' @export
#' 
#' 
#' @importFrom stats terms as.formula delete.response model.frame model.matrix coef
#' 
predict.dfunc <- function(object
                        , newdata = NULL
                        , type = c("parameters")
                        , distances = NULL
                        , propUnitSurveyed = 1.0
                        , area = NULL
                        , ...) {

  if (!inherits(object, "dfunc")){ 
    stop("Argument 'object' is not a 'dfunc' object")
  }
  
  isSmooth <- Rdistance::is.smoothed(object)
  
  # Establish the X matrix ----
  # We ALWAYS have covariates
  # We always need parameters, except for smoothed dfuncs
  if( !isSmooth ){
    if ( is.null(newdata) | (type == "likelihood") ) {
      # Case: Use original covars
      X <- model.matrix(object)
    } else {
      # Pull formula to make covars from NEWDATA
      Terms <- terms( object$mf )
      Terms <- delete.response( Terms ) # model.frame (below) fails if there's a response, go figure.
      if( !is.null(attr(Terms, "offset")) ){
        # gotta add a fake groupsize to newdata so model.frame (below) works
        # but, model.matrix drops offset, so this is silly
        gsName <- all.vars(Terms)[ attr(Terms, "offset") ]
        newdata <- cbind(newdata, data.frame( 1 ))
        names(newdata)[length(names(newdata))] <- gsName
      }
      xLevs <- lapply( object$mf, levels ) # get unspecified levels of factors
      m <- model.frame( Terms, newdata, xlev = xLevs )
      X <- model.matrix( Terms, m, contrasts.arg = attr(object$mf,"contrasts") )
    }
    
    BETA <- coef(object)
    p <- length(BETA)
    q <- ncol(X)
    paramsLink <- X %*% BETA[1:q] # could be extra parameters tacked on. e.g., knee for logistc or expansion terms
  
    if(q < p){
      extraParams <- matrix(BETA[(q+1):p]
                          , nrow = nrow(paramsLink)
                          , ncol = p-q
                          , byrow = TRUE)
      paramsLink <- cbind(paramsLink, extraParams)
    } else {
      extraParams <- NULL
    }
    
  } 

  # Dispatch functions to deal with 'type' ----
  if(type == "parameters" & isSmooth ){
    type = "dfuncs"  # smoothed dfuncs have no parameters
  }
  
  return(
    switch(type
         , dfunc = 
         , dfuncs = predDfuncs(object = object
                             , params = paramsLink
                             , distances = distances
                             , isSmooth = isSmooth) 
         , likelihood = predLikelihood(object = object
                                     , params = paramsLink
                                      ) 
         , abundance = 
         , density = predDensity(object = object
                               , propUnitSurveyed = propUnitSurveyed
                                )  
         , {  # the default = parameters
             cbind(exp(paramsLink[,1]), # All link functions are exp...thus far
                   extraParams )
           }
    )
  )
}
