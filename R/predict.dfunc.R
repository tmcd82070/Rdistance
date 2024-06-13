#' @title predict.dfunc - Predict distance functions
#' 
#' @description Predict either likelihood parameters for 
#' actual distance functions from estimated distance function 
#' objects.
#' 
#' @param object An estimated detection function, normally 
#' produced by calling \code{\link{dfuncEstim}}. 
#' 
#' @param newdata A data frame containing new values of 
#' the covariates at which to evaluate the distance functions. 
#' If \code{newdata}
#' is NULL, distance functions are evaluated at values of 
#' the observed covariates and results in one prediction 
#' (either parameters or 
#' distance function, see parameter \code{type}) for 
#' every observed distance. 
#' If \code{newdata} is not NULL and the model does not contains covariates, 
#' this routine returns one prediction (either parameters or 
#' distance function) for each row in \code{newdata}, but 
#' columns and values in \code{newdata} are ignored. 
#' 
#' @param type The type of predictions desired. 
#' \itemize{
#'   \item \bold{If \code{type} = "parameters"}: Returned values are
#'     predicted (canonical) parameters of the likelihood function. 
#'     If \code{newdata} is NULL, return contains one parameter
#'     value for every detection in \code{object} with a distance.
#'     If \code{newdata} is not NULL, returned vector has one parameter
#'     for every row in \code{newdata}.
#'   \item \bold{If \code{type} is not "parameters"}: Returned  
#'     value is a matrix of scaled distance functions, 
#'     one distance function per row. Distance functions 
#'     are evaluated at distances   
#'     specified in argument \code{distances}, not at the observed 
#'     distances in \code{object}. The number of distance functions
#'     returned depends on \code{newdata}: 
#'     \itemize{
#'        \item If \code{newdata} is NULL, one distance function 
#'        will be returned for every detected target in \code{object}
#'        that has valid covariate values. 

#'       \item If \code{newdata} is not NULL, one distance function 
#'       will be returned for each observation (row) in \code{newdata}. 
#'     }
#'  }
#'  
#'  If \code{object} is a smoothed distance function, it does not have parameters
#'  and this routine will always return a scaled distance function. That is, 
#'  \code{type} = "parameters" when \code{object} is smoothed 
#'  does not make sense and the smoothed distance function estimate will be returned. 
#' 
#' @param distances A vector of distances at which to evaluate 
#' distance functions, when distance functions 
#' are requested.  \code{distances} must have measurement units. 
#' Any distances outside the observation 
#' strip (\code{object$w.lo} to \code{object$w.hi}) are discarded.  If 
#' \code{distances} is NULL, this routine uses a sequence of 201 evenly 
#' spaced distances between 
#' \code{object$w.lo} and \code{object$w.hi}, inclusive. 
#'
#' @param \dots Included for compatibility with generic \code{predict} methods.
#' 
#' @return A matrix containing one of two types of predictions: 
#' \itemize{
#'   \item \bold{If \code{type} is "parameters"}, the returned matrix 
#'   contains predicted likelihood parameters. The extent of the 
#'   first dimension (rows) in 
#'   the returned matrix is equal to either the number of detection distances 
#'   in \code{object} 
#'   or number of rows in \code{newdata}. 
#'   The returned matrix's second dimension (columns) is 
#'   the number of parameters in the likelihood 
#'   plus the number of expansion terms.  Without expansion terms, the number 
#'   of columns in the returned matrix 
#'   is either 1 or 2 depending on the likelihood (e.g., \code{halfnorm} has 
#'   one parameter, \code{hazrate} has two). See the help 
#'   for each likelihoods to interpret the returned parameter values.
#'   
#'   \item \bold{If \code{type} is not "parameters"}, rows of the 
#'   returned matrix contain scaled distance functions.  The extent of the second 
#'   dimension (number of columns) is either the number of distances 
#'   specified in \code{distance}
#'   or 200 if \code{distances} is not specified.
#'   The extent of the first dimension (number of rows) is: 
#'     \itemize{
#'       \item the number of detections with distances: if \code{newdata} is NULL.
#'       \item the number of rows in \code{newdata}: if 
#'        \code{newdata} is specified.
#'     }
#'   All distance functions in columns of the return are scaled 
#'   to \code{object$g.x.scale} at \code{object$x.scl}.
#'   
#'   When \code{type} is not "parameters", the returned matrix has 
#'   additional attributes. 
#'   \code{attr(return, "x0")} is the vector of distances at which each 
#'   distance function in \code{return} was scaled. i.e., the vector of 
#'   \code{x.scl}.
#'   \code{attr(return, "scaler")} is a vector of scaling factors  
#'   corresponding to each 
#'   distance function in \code{return}. i.e., the vector of 
#'   \code{1/f(x.scl)} where \code{f()} is the un-scaled distance function. 
#'   If \code{object} contains line transects, \code{attr(return, "scaler")}
#'   is the vector of ESW corresponding to each distance function.
#' }
#' 
#' @seealso \code{\link{halfnorm.like}}, \code{\link{negexp.like}}, 
#' \code{\link{uniform.like}}, \code{\link{hazrate.like}}, \code{\link{Gamma.like}}
#' 
#' @examples
#' 
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
#' 
#' d <- units::set_units(c(0, 20, 40), "ft")
#' predict(dfuncObs, distances = d, type = "dfunc") 
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

  if (!inherits(object, "dfunc")){ 
    stop("Object is not a 'dfunc' object")
  }
  
  isSmooth <- Rdistance::is.smoothed(object)
  

  # if (missing(newdata) || is.null(newdata)) {
  #   n <- nrow(object$mf)
  # } else {
  #   n <- nrow(newdata)
  # }

  # Establish the X matrix ----
  # We ALWAYS have covariates
  # We always need parameters, except for smoothed dfuncs
  if( !isSmooth ){
    if (missing(newdata) || is.null(newdata)) {
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
    beta <- BETA[1:ncol(X)]   # could be extra parameters tacked on. e.g., knee for logistc or expansion terms
    paramsLink <- X %*% beta
    params <- exp(paramsLink)  # All link functions are exp...thus far
    
    if(ncol(X)<length(BETA)){
      extraParams <- matrix(BETA[(ncol(X)+1):length(BETA)]
                          , nrow = n
                          , ncol = length(BETA)-ncol(X)
                          , byrow = TRUE)
      params <- cbind(params, extraParams)
    }
  } 
    
  if( type == "parameters" & !isSmooth ){
    return(params)
  }

  # DISTANCE function prediction ----
  
  if( is.null(distances) ){
    distances <- seq( object$w.lo
                    , object$w.hi
                    , length = getOption("Rdistance_intEvalPts"))
  } else {
    # check distances have units
    if( !inherits(distances, "units") ){
      stop("Distances must have measurement units.")
    }
    # Make sure input distances are converted properly b.c. likelihoods drop units.
    distances <- units::set_units(distances, object$outputUnits, mode = "standard")
  }
  
  
  if( isSmooth ){
    # unscaled distance function is already stored
    # no covariates.  
    # NEED TO REVISIT THIS
    y <- stats::approx( object$fit$x, object$fit$y, xout = distances )$y
    y <- matrix( y, nrow = 1) 
  } else {
    # After next apply, y is length(distances) x nrow(parms).  
    # Each row is a un-scaled distance function (f(x))
    # We already eval-ed covars, so new X is constant (1), "XIntOnly"
    
    like <- match.fun( paste( object$likelihood, ".like", sep=""))
    XIntOnly <- matrix(1, nrow = length(distances), ncol = 1)
    y <- lapply(X = paramsLink
               , FUN = like
               , dist = distances - object$w.lo
               , covars = XIntOnly
    )  
    y <- lapply(y, function(x){x$L.unscaled})
    y <- do.call(rbind, y)  
    
    if(object$expansions > 0){
      # need null model with new responses 
      ml <- model.frame( distances ~ 1 )
      obj <- object
      obj$mf <- ml
      exp.terms <- Rdistance::expansionTerms(BETA, obj)
      y <- t( t(y) * exp.terms ) 
    }
  }
    
  # At this point, we have unscaled distance functions in rows of y.
  
  # SCALE distance functions ----
    
  if( object$likelihood == "Gamma" & !isSmooth ){
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
    # THE GAMMA CASE NEEDS CHECKED, MAYBE PUT THIS IN ANOTHER ROUTINE.
    maximize.g.reparam <- function( covRow, fit ){
      # On entry from apply(), covRow is a dimensionless vector. It must be a
      # matrix when we call the likelihood.
      covRow <- matrix(covRow, nrow = 1)
      F.maximize.g(fit, covars = covRow)
    }
    
    x0 <- apply(X = X
                     , MARGIN = 1
                     , FUN = maximize.g.reparam
                     , fit = object
                )
    # x0 is a distance, needs units
    x0 <- units::set_units(x0, object$outputUnits, mode = "standard")
    
  } else if( !isSmooth ){
    # Case:  All likelihoods except Gamma
    x0 <- rep(object$x.scl, nrow(params))
  } 
  
  # Now that we know x0 (either a scaler or a vector), compute f(x0)
  if( !isSmooth ){
    likeAtX0 <- function(i, params, x0, like, fit ){
      fx0 <- like(
          a = params[i,]
        , dist = x0[i] - fit$w.lo
        , covars = matrix(1,nrow = 1,ncol = 1)
      ) 
      fx0$L.unscaled
    }
    
    f.at.x0 <- sapply(1:nrow(params)
                      , FUN = likeAtX0
                      , params = params
                      , x0 = x0
                      , like = like
                      , fit = object
                )

    if(object$expansions > 0){
      # need null model with new responses 
      ml <- model.frame( x0 ~ 1 )
      obj <- object
      obj$mf <- ml
      exp.terms <- Rdistance::expansionTerms(BETA, obj)
      f.at.x0 <- f.at.x0 * exp.terms
    }
    

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
  
  attr(y, "x0") <- x0
  if(isSmooth){
    scaler <- scaler / 2
  }
  attr(y, "scaler") <- scaler

  
  return( y )  
}
