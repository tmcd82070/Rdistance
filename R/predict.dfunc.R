#' @title Predict method for dfunc objects
#' 
#' @description Predict likelihood parameters for distance function objects
#' 
#' @param object An estimated dfunc object.  See \code{dfuncEstim}. 
#' 
#' @param newdata A data frame containing new values of 
#' the covariates at which predictions are to be computed. If \code{newdata}
#' is NULL, predictions are made at values of the observed
#' covariates and results in one prediction (either parameters or 
#' distance function, see parameter \code{type}) for every observed distance. 
#' If \code{newdata} is not NULL and the model does not contains covariates, 
#' this routine returns one prediction (either parameters or 
#' distance function) for each row in \code{newdata}, but 
#' columns and values in \code{newdata} are ignored. 
#' 
#' @param type The type of predictions desired. 
#' \itemize{
#'   \item \bold{If \code{type} = "parameters"}: Return 
#'     predicted parameters of the likelihood function, one value for each observation
#'     (row) in \code{newdata}.  If \code{newdata} is NULL, return one predicted parameter
#'     value for every detection in \code{object$detections}.
#'   \item \bold{If \code{type} is not "parameters"}: Return  
#'     scaled distance functions. Distance functions are evaluated at the distances   
#'     specified in \code{distances}. The number of distance functions returned 
#'     depends on \code{newdata} and whether \code{object} contains covariates:
#'     \itemize{
#'       \item If \code{object} does NOT contain covariates, the distance 
#'       function does not vary (by covariate) and only one distance function
#'       will be returned, even if \code{newdata} is specified.
#'       \item If \code{object} contains covariates, one distance function 
#'       will be returned for each observation (row) in \code{newdata}. 
#'       If \code{newdata} is NULL, one distance function will be returned 
#'       for every detection in \code{object$detections}. 
#'     }
#'  }
#'  If \code{object} is a smoothed distance function, it does not have parameters
#'  and this routine will always return a scaled distance function. That is, 
#'  \code{type} = "parameters" when \code{object} is smoothed 
#'  does not make sense and the smoothed distance function estimate will be returned. 
#' 
#' @param distances A vector of distances when distance functions 
#' are requested.  \code{distances} must have measurement units. 
#' Any distances outside the observation 
#' strip (\code{object$w.lo} to \code{object$w.hi}) are discarded.  If 
#' \code{distances} is NULL, this routine uses a sequence of 200 evenly 
#' spaced distances between 
#' \code{object$w.lo} and \code{object$w.hi}, inclusive 
#'
#' @param \dots Included for compatibility with generic \code{predict} methods.
#' 
#' @return A matrix containing one of two types of predictions: 
#' \itemize{
#'   \item \bold{If \code{type} is "parameters"}, the returned matrix 
#'   contains predicted likelihood parameters. The extent of the first dimension (rows) in 
#'   the returned matrix is equal to either the number of detection distances 
#'   in \code{object$detections} 
#'   or number of rows in \code{newdata}. 
#'   The returned matrix's second dimension (columns) is 
#'   the number of parameters in the likelihood 
#'   plus the number of expansion terms.  Without expansion terms, the number 
#'   of columns in the returned matrix 
#'   is either 1 or 2 depending on the likelihood (e.g., \code{halfnorm} has 
#'   one parameter, \code{hazrate} has two). See the help 
#'   for each likelihoods to interpret the returned parameter values.
#'   
#'   \item \bold{If \code{type} is not "parameters"}, the returned matrix 
#'   contains scaled distance functions.  The extent of the first 
#'   dimension (rows) is either the number of distances specified in \code{distance}
#'   or 200 if \code{distances} is not specified.
#'   The extent of the second dimension (columns) is: 
#'     \itemize{
#'       \item 1: if \code{object} does NOT contain covariates.
#'       \item the number of detections: if \code{object} contains covariates and \code{newdata} is NULL.
#'       \item the number of rows in \code{newdata}: if \code{object} contains covariates 
#'       and \code{newdata} is specified.
#'     }
#'   All distance functions in columns of the return are scaled 
#'   to \code{object$g.x.scale} at \code{object$x.scl}.
#'   
#'   When \code{type} is not "parameters", the returned matrix has 
#'   additional attributes containing the distances 
#'   at which the functions are scaled and ESW's.  
#'   \code{attr(return, "x0")} is the vector of distances at which each 
#'   distance function in \code{<return>} is scaled. i.e., the vector of 
#'   \code{x.scl}.
#'   \code{attr(return, "scaler")} is a vector scaling factors  
#'   corresponding to each 
#'   distance function in \code{return}. i.e., the vector of 
#'   \code{1/f(x.scl)} where \code{f()} is the unscaled distance function. 
#'   If \code{object} contains line transects, \code{attr(return, "scaler")}
#'   is a vector of ESW corresponding to each distance function.
#' }
#' 
#' @seealso \code{\link{halfnorm.like}}, \code{\link{negexp.like}}, 
#' \code{\link{uniform.like}}, \code{\link{hazrate.like}}, \code{\link{Gamma.like}}
#' 
#' @examples
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' # No covariates
#' dfuncObs <- dfuncEstim(formula = dist ~ 1
#'                      , detectionData = sparrowDetectionData
#'                      , w.hi = units::as_units(100, "m"))
#' predict(dfuncObs)
#' # values in newdata ignored because no covariates
#' predict(dfuncObs, newdata = data.frame(x = 1:5)) 
#' 
#' predict(dfuncObs, type = "dfunc") # one function
#' predict(dfuncObs, newdata = data.frame(x = 1:5), type = "dfunc") # same
#' 
#' d <- units::set_units(c(0, 20, 40), "ft")
#' predict(dfuncObs, distances = d, type = "dfunc") 
#' d <- units::set_units(c(0.000,  6.096, 12.192), "m")
#' predict(dfuncObs, distances = d, type = "dfunc") # same 
#' 
#' # Covariates
#' dfuncObs <- dfuncEstim(formula = dist ~ observer
#'                      , detectionData = sparrowDetectionData
#'                      , siteData = sparrowSiteData
#'                      , w.hi = units::as_units(100, "m"))
#' predict(dfuncObs)  # 356 X 1
#' 
#' Observers <- data.frame(observer = levels(sparrowSiteData$observer))
#' predict(dfuncObs, newdata = Observers) # 5 X 1
#' 
#' predict(dfuncObs, type = "dfunc") # 200 X 356
#' predict(dfuncObs, newdata = Observers, type = "dfunc") # 200 X 5
#' predict(dfuncObs, newdata = Observers, distances = d, type = "dfunc") # 3 X 5
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
                        , ...) {
  
  is.smoothed <- class(object$fit) == "density"
  
  if (!inherits(object, "dfunc")){ 
    stop("Object is not a 'dfunc' object")
  }
  
  hasCovars <- !is.null(object$covars)
  
  if (missing(newdata) || is.null(newdata)) {
    n <- nrow(object$detections)
  } else {
    n <- nrow(newdata)
  }
  
  # PARAMETER prediction ----
  # We always need parameters, except for smoothed dfuncs
  if( hasCovars & !is.smoothed){
    # X is the covariate matrix for predictions 
    if (missing(newdata) || is.null(newdata)) {
      # Case: Use original covars
      X <- object$covars
    } else {
      # Pull formula to make covars from NEWDATA
      Terms <- terms( object$model.frame )
      Terms <- delete.response( Terms ) # model.frame (below) fails if there's a response, go figure.
      if( !is.null(attr(Terms, "offset")) ){
        # gotta add a fake groupsize to newdata so model.frame (below) works
        # but, model.matrix drops offset, so this is silly
        gsName <- all.vars(Terms)[ attr(Terms, "offset") ]
        newdata <- cbind(newdata, data.frame( 1 ))
        names(newdata)[length(names(newdata))] <- gsName
      }
      xLevs <- lapply( object$model.frame, levels )
      m <- model.frame( Terms, newdata, xlev = xLevs )
      X <- model.matrix( Terms, m, contrasts.arg = attr(object$covars,"contrasts") )
    }
    
    BETA <- coef(object)
    beta <- BETA[1:ncol(X)]   # could be extra parameters tacked on. e.g., knee for logistc or expansion terms
    params <- X %*% beta
    params <- exp(params)  # All link functions are exp...thus far
    
    if(ncol(X)<length(BETA)){
      extraParams <- matrix(BETA[(ncol(X)+1):length(BETA)]
                          , nrow = n
                          , ncol = length(BETA)-ncol(X)
                          , byrow = TRUE)
      params <- cbind(params, extraParams)
    }
  } else if( !is.smoothed ){
    params <- coef(object) 
    params <- matrix(params, nrow=n, ncol=length(params), byrow=TRUE)
  }
    
  if( type == "parameters" & !is.smoothed ){
    return(params)
  }

  # DISTANCE function prediction ----

  if( is.null(distances) ){
    distances <- seq( object$w.lo, object$w.hi, length = 200)
  } else {
    # check distances have units
    if( !inherits(distances, "units") ){
      stop("Distances must have measurement units.")
    }
    # Make sure input distances are converted properly b.c. likelihoods drop units.
    distances <- units::set_units(distances, object$outputUnits, mode = "standard")
  }
  
  if( is.smoothed ){
    # unscaled distance function is already stored
    # no covariates.  
    y <- stats::approx( object$fit$x, object$fit$y, xout = distances )$y
    y <- matrix( y, nrow = 1) 
  } else {
    # After next apply, y is length(distances) x nrow(parms).  
    # Each column is a unscaled distance function (f(x))
    
    like <- match.fun( paste( object$like.form, ".like", sep=""))
    zero <- units::as_units(0, object$outputUnits)
    
    if( !hasCovars ){
      # cut the params down because everything is constant without covars
      params <- params[1, , drop = FALSE]
      X <- matrix(1, nrow = 1, ncol = 1) 
    }
    
    y <- apply(X = params
               , MARGIN = 1
               , FUN = like
               , dist = distances - object$w.lo
               , series = object$series
               , covars = NULL
               , expansions = object$expansions
               , w.lo = zero
               , w.hi = object$w.hi - object$w.lo
               , pointSurvey = FALSE
               , scale = TRUE
    )  
      
    y <- t(y)  # now, each row of y is a dfunc
  }
    
  # At this point, we have unscaled distance functions in rows of y.
  
  # SCALE distance functions ----
    
  if( object$like.form == "Gamma" & !is.smoothed ){
    # This is a pesky special case of scaling. We need different x.scl for 
    # every distance function b.c. only x.scl = max is allowed.  For all other 
    # distance functions there is only one x.scl.
    # We could do all distance functions this way, i.e., call F.gx.estim
    # for every value of params, but it is faster not to. For all but Gamma,
    # we computed x.scl in dfuncEstim and it's constant.
    #
    # Cannot take apply(y,1,max) because maybe only 1 or 2 distances are predicted.
    # Cannot compute mode of Gamma (lam * b * (r-1)) because there might be extensions,
    # which change the mode.
    # Must call maximize.g which uses optim to find maximum.
    #
    maximize.g.reparam <- function( covRow, fit, hasCovars ){
      if( hasCovars ){
        # On entry from apply(), covRow is a dimensionless vector. It must be a
        # matrix when we call the likelihood.
        covRow <- matrix(covRow, nrow = 1)
      } else {
        covRow <- NULL
      }
      F.maximize.g(fit, covars = covRow)
    }
    
    x0 <- apply(X = X
                     , MARGIN = 1
                     , FUN = maximize.g.reparam
                     , fit = object
                     , hasCovars = hasCovars
                )
    # x0 is a distance, needs units
    x0 <- units::set_units(x0, object$outputUnits, mode = "standard")
    
  } else if( !is.smoothed ){
    # Case:  All likelihoods except Gamma
    x0 <- rep(object$x.scl, nrow(params))
  } 
  
  # Now that we know x0 (either a scaler or a vector), compute f(x0)
  if( !is.smoothed ){
    likeAtX0 <- function(i, params, x0, like, fit ){
      fx0 <- like(
          a = params[i,]
        , dist = x0[i] - fit$w.lo
        , series = fit$series
        , covars = NULL
        , expansions = fit$expansions
        , w.lo = zero
        , w.hi = fit$w.hi - fit$w.lo
        , pointSurvey = FALSE 
        , scale = TRUE
      ) 
      fx0
    }
    
    f.at.x0 <- sapply(1:nrow(params)
                      , FUN = likeAtX0
                      , params = params
                      , x0 = x0
                      , like = like
                      , fit = object
                )
    
    # y <- t(y) # for some reason, we go back to columns. Each column is a dfunc, only now scaled.
    
  } else {
    # Case: smoothed distance function.  No covars. 
    # This is so simple we handle everything here
    x0 <- object$x.scl
    f.at.x0 <- stats::approx(distances, y, xout = x0)$y 
  }
    

  scaler <- object$g.x.scl / f.at.x0 # a length n vector, n = nrow(params) 

  # Did you know that 'scaler' is ESW?  At least for lines. Makes sense. 1/f(0) = ESW in 
  # the old formulas.
  # For smoothed distance functions, we extended the range of dist by approx 2, and scaler
  # is twice too big. i.e., esw for smu's is approx scaler / 2.
  
  y <- y * scaler  # length(scalar) == nrow(y), so this works right
  
  # Until now, rows of y were distance functions. Because other routines expect it, 
  # and for matplot ease, return distance functions in columns
  y <- t(y) 
  
  attr(y, "x0") <- x0
  if(is.smoothed){
    scaler <- scaler / 2
  }
  attr(y, "scaler") <- scaler

  
  return( y )  
}
