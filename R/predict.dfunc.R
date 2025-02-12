#' @title predict.dfunc - Predict distance functions
#' 
#' @description Predict either likelihood parameters, 
#' distance functions, site-specific density, or 
#' site-specific abundance from estimated distance function 
#' objects.
#' 
#' @param x An estimated detection function object, normally 
#' produced by calling \code{\link{dfuncEstim}}. 
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
#'     one parameter value for every non-missing detection distance
#'     in \code{x}. If \code{newdata} is not NULL, returned vector 
#'     has one parameter for every row in \code{newdata}. Parameter 
#'     \code{distances} is ignored in this case. Canonical parameters 
#'     (non-expansion terms)
#'     are returned on the response (inverse-link) scale.  Raw 
#'     canonical parameters in \code{x$par} are stored in 
#'     the link scale.  Expansion term parameters use the identity 
#'     link, so their value in the output equals their value 
#'     in \code{x$par}. 
#'     
#'   \item \bold{If \code{type} == "likelihood"}: Returned value is a 
#'     matrix of \bold{unscaled} likelihood values for all non-missing 
#'     observed distances in \code{x}, i.e., raw distance functions
#'     evaluated at the observed distances.  Parameters \code{newdata} and 
#'     \code{distances} are ignored when \code{type} is "likelihood". 
#'     The negative log likelihood of the full data set is
#'     \code{-sum(log(predict(x,type="likelihood") / effectiveDistance(x)))}. 
#'     
#'   \item \bold{If \code{type} == "dfuncs"}: Returned  
#'     value is a matrix whose columns contain scaled distance functions. 
#'     The distance functions in each column are evaluated at distances   
#'     in argument \code{distances}, not at the observed 
#'     distances in \code{x}. The number of distance functions
#'     returned (i.e., number of columns) depends on \code{newdata} 
#'     as follows: 
#'     \itemize{
#'        \item If \code{newdata} is NULL, one distance function 
#'        will be returned for every detection in \code{x}
#'        that has valid covariate values. Observation distances 
#'        in \code{x} are ignored in this case; hence, a distance 
#'        function is estimated for all observations, even
#'        those with missing detection distances.   
#'       \item If \code{newdata} is not NULL, one distance function 
#'       will be returned for each observation (row) in \code{newdata}. 
#'     }
#'     
#'    \item \bold{If \code{type} == "density"}: Returned object is a data frame
#'    containing one row per transect. Columns in the data frame include 
#'    transect ID, which identifies transects, and estimated density on 
#'    the area sampled by the transect. 
#'    
#'    \item \bold{If \code{type} == "abundance"}: Returned object is 
#'    a data frame containing one row per transect and 
#'    estimated values for abundance on each transect. 
#'    Abundance estimates are density on the transect's sampled 
#'    area multiplied by size of the sampled area (i.e., 
#'    length times nominal width = length(!singleSided+1)(w.hi - w.lo)).
#'  }  
#'  
#'  If \code{x} is a smoothed distance function, it does not have parameters
#'  and this routine will only return scaled distance functions, densities, or 
#'  abundances. That is, 
#'  \code{type} = "parameters" when \code{x} is smoothed 
#'  does not make sense and the smoothed distance function estimate 
#'  will be returned if \code{type} does not equal "density" or "abundance". 
#' 
#' @param distances A vector or matrix of distances at which to evaluate 
#' distance functions, when distance functions 
#' are requested.  \code{distances} must have measurement units. 
#' Any distances outside the observation 
#' strip (\code{x$w.lo} to \code{x$w.hi}) are discarded.  If 
#' \code{distances} is NULL, this routine predicts at a sequence 
#' of \code{getOption("Rdistance_intEvalPts")} (default 101) evenly 
#' spaced distances between 
#' \code{x$w.lo} and \code{x$w.hi}, inclusive. 
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
#'   in \code{x} or number of rows in \code{newdata}. 
#'   The returned matrix's second dimension (columns) is 
#'   the number of parameters in the likelihood 
#'   plus the number of expansion terms.  
#'   See the help for each likelihoods to interpret  
#'   returned parameter values. All parameters are returned 
#'   on the inverse-link scale; i.e., \emph{exponential} for canonical 
#'   parameters and \emph{identity} for expansion terms. 
#'   
#'   \item \bold{If \code{type} is "dfuncs"}, columns of the 
#'   returned matrix contains detection functions (i.e., \emph{g(x)}).  
#'   The extent of the first 
#'   dimension (number of rows) is either the number of distances 
#'   specified in \code{distances}
#'   or \code{options()$Rdistance_intEvalPts} if \code{distances} is 
#'   not specified.
#'   The extent of the second dimension (number of columns) is: 
#'     \itemize{
#'       \item the number of detections with non-missing distances: if \code{newdata} is NULL.
#'       \item the number of rows in \code{newdata} if 
#'        \code{newdata} is specified.
#'     }
#'   All distance functions in columns of the return are scaled 
#'   to \code{x$g.x.scale} at \code{x$x.scl}. The returned matrix has 
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
#'   \item \bold{If \code{type} is "density"}...
#'   
#'   \item \bold{If \code{type} is "abundance"}...
#' }
#' 
#' @seealso \code{\link{halfnorm.like}}, \code{\link{negexp.like}}, 
#' \code{\link{uniform.like}}, \code{\link{hazrate.like}}, \code{\link{Gamma.like}}
#' 
#' @examples
#' 
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#'
#' # For dimension checks:
#' nd <- getOption("Rdistance_intEvalPts")
#' n  <- nrow(dfuncObs$mf)
#' 
#' # No covariates
#' dfuncObs <- sparrowDf |> dfuncEstim(formula = dist ~ 1
#'                      , w.hi = units::as_units(100, "m"))
#'                      
#' p <- predict(dfuncObs) # parameters
#' all(dim(p) == c(nd, 1)) 
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
#' Observers <- data.frame(observer = levels(sparrowSiteData$observer))
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
predict.dfunc <- function(x
                        , newdata = NULL
                        , type = c("parameters")
                        , distances = NULL
                        , singleSided = FALSE
                        , ...) {

  if (!inherits(x, "dfunc")){ 
    stop("Argument 'x' is not a 'dfunc' object")
  }
  
  isSmooth <- Rdistance::is.smoothed(x)
  
  # Establish the X matrix ----
  # We ALWAYS have covariates
  # We always need parameters, except for smoothed dfuncs
  if( !isSmooth ){
    if ( is.null(newdata) | (type == "likelihood") ) {
      # Case: Use original covars
      X <- model.matrix(x)
    } else {
      # Pull formula to make covars from NEWDATA
      Terms <- terms( x$mf )
      Terms <- delete.response( Terms ) # model.frame (below) fails if there's a response, go figure.
      if( !is.null(attr(Terms, "offset")) ){
        # gotta add a fake groupsize to newdata so model.frame (below) works
        # but, model.matrix drops offset, so this is silly
        gsName <- all.vars(Terms)[ attr(Terms, "offset") ]
        newdata <- cbind(newdata, data.frame( 1 ))
        names(newdata)[length(names(newdata))] <- gsName
      }
      xLevs <- lapply( x$mf, levels ) # get unspecified levels of factors
      m <- model.frame( Terms, newdata, xlev = xLevs )
      X <- model.matrix( Terms, m, contrasts.arg = attr(x$mf,"contrasts") )
    }
    
    BETA <- coef(x)
    p <- length(BETA)
    q <- ncol(X)
    paramsLink <- X %*% BETA[1:q] # could be extra parameters tacked on. e.g., knee for logistc or expansion terms
    
    if(q < p){
      extraParams <- matrix(BETA[(q+1):p]
                          , nrow = nrow(paramsLink)
                          , ncol = p-q
                          , byrow = TRUE)
      paramsLink <- cbind(paramsLink, extraParams)
    }
  } 

  # Dispatch functions to deal with 'type' ----
  if(type == "parameters" & isSmooth ){
    type = "dfuncs"  # smoothed dfuncs have no parameters
  }
  
  return(
    switch(type,
           dfuncs = predict.dfunc.dfuncs(x = x
                                         , params = paramsLink
                                         , distances = distances
                                         , isSmooth = isSmooth) , 
           likelihood = predict.dfunc.likelihood(x = x
                                               , params = paramsLink
                                               ) , 
           abundance = ,
           density = predict.dfunc.density(x = x
                                         , params = paramsLink
                                         , type = type
                                         ) , 
           {  # the default = parameters
             cbind(exp(paramsLink[,1:q]), # All link functions are exp...thus far
                   extraParams )
           }
    )
  )
}
