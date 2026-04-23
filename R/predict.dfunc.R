#' @title Predict distance functions
#' 
#' @description Predict either likelihood parameters, 
#' distance functions, site-specific density, or 
#' site-specific abundance from estimated distance function 
#' objects.
#' 
#' @param object An Rdistance model frame or fitted distance function,
#' normally produced by a call to [dfuncEstim()]. 
#' 
#' @param newdata A data frame containing new values of 
#' the covariates at which to evaluate the distance functions. 
#' If `newdata`
#' is NULL, distance functions are evaluated at values of 
#' the observed covariates and results in one prediction 
#' per distance or transect (see parameter `type`). 
#' If `newdata` is not NULL and the model does not contains covariates, 
#' this routine returns one prediction for each row in `newdata`, but 
#' columns and values in `newdata` are ignored. 
#' 
#' @param type The type of predictions desired. 
#' \itemize{
#'   \item **If `type` == "parameters"**: Returned value is 
#'     a matrix of predicted (canonical) parameters of the 
#'     likelihood function. If `newdata` is NULL, return contains 
#'     one parameter value for every detection distance
#'     in `object$mf` (distances in `object$mf` are between 
#'     `object$w.lo` and `object$w.hi` and non-missing). 
#'     If `newdata` is not NULL, returned vector 
#'     has one parameter for every row in `newdata`. Parameter 
#'     `distances` is ignored when `type` == "parameters". 
#'     Canonical parameters (non-expansion terms)
#'     are returned on the response (inverse-link) scale.  Raw 
#'     canonical parameters in `object$par` are stored in 
#'     the link scale.  Expansion term parameters use the identity 
#'     link, so their value in the output equals their value 
#'     in `object$par`. 
#'     
#'   \item **If `type` == "likelihood"**: Returned value is a 
#'     matrix of **unscaled** likelihood values for all  
#'     observed distances in `object$mf`, i.e., raw distance functions
#'     evaluated at the observed distances.  Parameters `newdata` and 
#'     `distances` are ignored when `type` is "likelihood". 
#'     The negative log likelihood of the full data set is
#'     `-sum(log(predict(object,type="likelihood") / 
#'     effectiveDistance(object)))`. 
#'     
#'   \item **If `type` == "dfuncs" or "dfunc"**: Returned  
#'     value is a matrix whose columns contain scaled distance functions. 
#'     The distance functions in each column are evaluated at distances   
#'     in argument `distances`, not at the observed 
#'     distances in `object$mf`. The number of distance functions
#'     returned (i.e., number of columns) depends on `newdata` 
#'     as follows: 
#'     \itemize{
#'        \item If `newdata` is NULL, one distance function 
#'        will be returned for every detection in `object$mf`
#'        that has valid covariate values.    
#'       \item If `newdata` is not NULL, one distance function 
#'       will be returned for each observation (row) in `newdata`. 
#'     }
#'     
#'    \item **If `type` == "density" or "abundance"**: Returned 
#'    object is a tibble containing predicted density and abundance 
#'    on the area surveyed by each transect. 
#'  }  
#'  
#'  If `object` is a smoothed distance function, it does not have parameters
#'  and this routine will only return scaled distance functions, densities, or 
#'  abundances. That is, 
#'  `type` = "parameters" when `object` is smoothed 
#'  does not make sense and the smoothed distance function estimate 
#'  will be returned if `type` does not equal "density" or "abundance". 
#' 
#' @param distances A vector or 1-column matrix of 
#' distances at which to evaluate 
#' distance functions, when distance functions 
#' are requested.  `distances` must have measurement units. 
#' Any distances outside the observation 
#' strip (`object$w.lo` to `object$w.hi`) are discarded.  If 
#' `distances` is NULL, a sequence 
#' of `getOption("Rdistance_intEvalPts")` (default 101) evenly 
#' spaced distances between 
#' `object$w.lo` and `object$w.hi` (inclusive) is used. 
#' 
#' @inheritParams abundEstim
#'
#' @param \dots Included for compatibility with generic `predict` methods.
#' 
#' @return A matrix containing predictions: 
#' \itemize{
#'   \item **If `type` is "parameters"**, the returned matrix 
#'   contains likelihood parameters. The extent of the 
#'   first dimension (rows) in the returned matrix is equal to 
#'   either the number of detection distances 
#'   in the observed strip or number of rows in `newdata`. 
#'   The returned matrix's second dimension (columns) is 
#'   the number of parameters in the likelihood 
#'   plus the number of expansion terms.  
#'   See the help for each likelihoods to interpret  
#'   returned parameter values. All parameters are returned 
#'   on the inverse-link scale; i.e., *exponential* for canonical 
#'   parameters and *identity* for expansion terms. 
#'   
#'   \item **If `type` is "dfuncs" or "dfunc"**, columns of the 
#'   returned matrix contains detection functions (i.e., *g(x)*).  
#'   The extent of the first 
#'   dimension (number of rows) is either the number of distances 
#'   specified in `distances`
#'   or `options()$Rdistance_intEvalPts` if `distances` is 
#'   not specified.
#'   The extent of the second dimension (number of columns) is: 
#'     \itemize{
#'       \item the number of detections with non-missing distances: 
#'       if `newdata` is NULL.
#'       \item the number of rows in `newdata` if 
#'        `newdata` is specified.
#'     }
#'   All distance functions in columns of the return are scaled 
#'   to `object$g.x.scale` at `object$x.scl`. The returned matrix has 
#'   the following additional attributes:
#'    \itemize{
#'       \item `attr(return, "distances")` is the vector of distances used to 
#'       predict the function in `return`.  Either the input `distances` object
#'       or the computed sequence of distances when `distances` is NULL. 
#'       \item `attr(return, "x0")` is the vector of distances at which each 
#'        distance function in `return` was scaled. i.e., the vector of 
#'        `x.scl`.
#'       \item `attr(return, "g.x.scl")` is the height of *g(x)* (the distance 
#'        function) at *x0*. 
#'   }
#'   
#'   \item **If `type` is "density" or "abundance"**, the return is a 
#'   tibble containing density and abundance estimates by transect.  
#'   All transects in the input data (i.e., `object$data`) are 
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
#' @seealso [halfnorm.like()], [negexp.like()], 
#' [hazrate.like()]
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
#' d <- setUnits(c(0, 20, 40), "ft")
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
#' data(sparrowDfuncObserver) # pre-estimated object
#' \dontrun{
#' # Command to generate 'sparrowDfuncObserver'
#' sparrowDfuncObserver <- sparrowDf |> 
#'             dfuncEstim(formula = dist ~ observer
#'                      , likelihood = "hazrate")
#' }
#' 
#' predict(sparrowDfuncObserver)  # n X 2
#' 
#' Observers <- data.frame(observer = levels(sparrowDf$observer))
#' predict(sparrowDfuncObserver, newdata = Observers) # 5 X 2
#' 
#' predict(sparrowDfuncObserver, type = "dfunc") # nd X n
#' predict(sparrowDfuncObserver, newdata = Observers, type = "dfunc") # nd X 5
#' d <- setUnits(c(0, 150, 400), "ft")
#' predict(sparrowDfuncObserver
#'   , newdata = Observers
#'   , distances = d
#'   , type = "dfunc") # 3 X 5
#' 
#' # Density and abundance by transect
#' predict(sparrowDfuncObserver
#'   , type = "density")
#'   
#' @export
#' 
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
      X <- stats::model.matrix(object)
    } else {
      # We have NEWDATA to deal with
      Terms <- stats::terms( object$mf )
      Terms <- stats::delete.response( Terms ) # model.frame (below) fails if there's a response, go figure.
      gsName <- all.vars(Terms)[ attr(Terms, "offset") ] # there is always an offset
      if( !(gsName %in% names(newdata)) ){
        # gotta add a fake groupsize to newdata so model.frame (below) works
        # but, model.matrix drops offset, so this is silly
        newdata <- cbind(newdata, data.frame( 1 ))
        names(newdata)[length(names(newdata))] <- gsName
      }
      xLevs <- lapply( object$mf, levels ) # get unspecified levels of factors
      m <- stats::model.frame( formula = Terms
                      , data = newdata
                      , xlev = xLevs )
      X <- stats::model.matrix( object = Terms
                       , data = m
                       , contrasts.arg = attr(object$mf,"contrasts") )
    }
    
    BETA <- stats::coef(object)
    p <- length(BETA)
    q <- ncol(X)
    
    paramsLink <- X %*% BETA[1:q] # could be extra parameters tacked on. e.g., shape or expansion terms
  
    if(q < p){
      extraParams <- matrix(BETA[(q+1):p]
                          , nrow = nrow(paramsLink)
                          , ncol = p-q
                          , byrow = TRUE)
      # types 'dfunc' and 'likelihood' need the extra params attached
      # other types do not; but, attach extras anyway
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
             parms <- cbind(exp(paramsLink[,1]), # All link functions are exp...thus far
                            extraParams )
             
             parmsNames <- likeParamNames(object$likelihood) # cannonical names
             if( object$expansions >= 1 ){
               parmsNames <- c(parmsNames, names(BETA)[p - (object$expansions:1) + 1])
             }
             dimnames(parms) <- list(rownames(newdata), parmsNames)
             parms
           }
    )
  )
}
