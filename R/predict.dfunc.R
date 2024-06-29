#' @title predict.dfunc - Predict distance functions
#' 
#' @description Predict either likelihood parameters for 
#' actual distance functions from estimated distance function 
#' objects.
#' 
#' @param x An estimated detection function, normally 
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
#'     value for every detection in \code{x} with a distance.
#'     If \code{newdata} is not NULL, returned vector has one parameter
#'     for every row in \code{newdata}.
#'   \item \bold{If \code{type} is not "parameters"}: Returned  
#'     value is a matrix of scaled distance functions, 
#'     one distance function per row. Distance functions 
#'     are evaluated at distances   
#'     specified in argument \code{distances}, not at the observed 
#'     distances in \code{x}. The number of distance functions
#'     returned depends on \code{newdata}: 
#'     \itemize{
#'        \item If \code{newdata} is NULL, one distance function 
#'        will be returned for every detected target in \code{x}
#'        that has valid covariate values. 

#'       \item If \code{newdata} is not NULL, one distance function 
#'       will be returned for each observation (row) in \code{newdata}. 
#'     }
#'  }
#'  
#'  If \code{x} is a smoothed distance function, it does not have parameters
#'  and this routine will always return a scaled distance function. That is, 
#'  \code{type} = "parameters" when \code{x} is smoothed 
#'  does not make sense and the smoothed distance function estimate will be returned. 
#' 
#' @param distances A vector of distances at which to evaluate 
#' distance functions, when distance functions 
#' are requested.  \code{distances} must have measurement units. 
#' Any distances outside the observation 
#' strip (\code{x$w.lo} to \code{x$w.hi}) are discarded.  If 
#' \code{distances} is NULL, this routine uses a sequence of 201 evenly 
#' spaced distances between 
#' \code{x$w.lo} and \code{x$w.hi}, inclusive. 
#'
#' @param \dots Included for compatibility with generic \code{predict} methods.
#' 
#' @return A matrix containing one of two types of predictions: 
#' \itemize{
#'   \item \bold{If \code{type} is "parameters"}, the returned matrix 
#'   contains predicted likelihood parameters. The extent of the 
#'   first dimension (rows) in 
#'   the returned matrix is equal to either the number of detection distances 
#'   in \code{x} 
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
#'   to \code{x$g.x.scale} at \code{x$x.scl}.
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
#'   If \code{x} contains line transects, \code{attr(return, "scaler")}
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
                        , ...) {

  if (!inherits(x, "dfunc")){ 
    stop("Argument 'x' is not a 'dfunc' object")
  }
  
  isSmooth <- Rdistance::is.smoothed(x)
  

  # if (missing(newdata) || is.null(newdata)) {
  #   n <- nrow(x$mf)
  # } else {
  #   n <- nrow(newdata)
  # }

  # Establish the X matrix ----
  # We ALWAYS have covariates
  # We always need parameters, except for smoothed dfuncs
  if( !isSmooth ){
    if ( is.null(newdata) ) {
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
    beta <- BETA[1:ncol(X)]   # could be extra parameters tacked on. e.g., knee for logistc or expansion terms
    paramsLink <- X %*% beta
    params <- exp(paramsLink)  # All link functions are exp...thus far
    
    if(ncol(X)<length(BETA)){
      extraParams <- matrix(BETA[(ncol(X)+1):length(BETA)]
                          , nrow = nrow(params)
                          , ncol = length(BETA)-ncol(X)
                          , byrow = TRUE)
      params <- cbind(params, extraParams)
      paramsLink <- cbind(paramsLink, extraParams)
    }
  } 
    
  if( type == "parameters" & !isSmooth ){
    return(params)
  }

  # DISTANCE function prediction ----
  
  if( is.null(distances) ){
    distances <- seq( x$w.lo
                    , x$w.hi
                    , length = getOption("Rdistance_intEvalPts"))
  } else {
    # check distances have units
    if( !inherits(distances, "units") ){
      stop("Distances must have measurement units.")
    }
    # Make sure input distances are converted properly b.c. likelihoods drop units.
    distances <- units::set_units(distances, x$outputUnits, mode = "standard")
  }
  
  
  if( isSmooth ){
    # unscaled distance function is already stored
    # no covariates.  
    # NEED TO REVISIT THIS
    y <- stats::approx( x$fit$x, x$fit$y, xout = distances )$y
    y <- matrix( y, nrow = 1) 
  } else {
    # After next apply, y is length(distances) x nrow(parms).  
    # Each row is a un-scaled distance function (f(x))
    # We already eval-ed covars, so new X is constant (1), "XIntOnly"
    
    like <- match.fun( paste( x$likelihood, ".like", sep=""))
    XIntOnly <- matrix(1, nrow = length(distances), ncol = 1)
    y <- apply(X = paramsLink
               , MARGIN = 1
               , FUN = like
               , dist = distances - x$w.lo
               , covars = XIntOnly
    )  
    y <- lapply(y, function(x){x$L.unscaled})
    y <- do.call(cbind, y)  
    
    if(x$expansions > 0){
      # need null model with new responses 
      ml <- model.frame( distances ~ 1 )
      obj <- x
      obj$mf <- ml
      exp.terms <- Rdistance::expansionTerms(BETA, obj)
      y <- y * exp.terms  
    }
  }
    
  # At this point, we have unscaled distance functions in columns of y.
  
  # SCALE distance functions ----
    
  if( x$likelihood == "Gamma" & !isSmooth ){
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
                     , fit = x
                )
    # x0 is a distance, needs units
    x0 <- units::set_units(x0, x$outputUnits, mode = "standard")
    
  } else if( !isSmooth ){
    # Case:  All likelihoods except Gamma
    x0 <- rep(x$x.scl, nrow(params))
  } 
  
  # Now that we know x0 (either a scaler or a vector), compute f(x0)
  
  if( !isSmooth ){
  
    # Likelihood functions return g(x). This is what we want.
    # likeAtX0 <- function(i, params, x0, like, fit ){
    #   fx0 <- like(
    #       a = params[i,]
    #     , dist = x0[i] - fit$w.lo
    #     , covars = matrix(1,nrow = 1,ncol = 1)
    #   ) 
    #   fx0$L.unscaled
    # }
    # 
    # f.at.x0 <- sapply(1:nrow(params)
    #                   , FUN = likeAtX0
    #                   , params = params
    #                   , x0 = x0
    #                   , like = like
    #                   , fit = x
    #             )
    # 
    # if(x$expansions > 0){
    #   # need null model with new responses 
    #   ml <- model.frame( x0 ~ 1 )
    #   obj <- x
    #   obj$mf <- ml
    #   exp.terms <- Rdistance::expansionTerms(BETA, obj)
    #   f.at.x0 <- f.at.x0 * exp.terms
    # }
    

  } else {
    # Case: smoothed distance function.  No covars. 
    # This is so simple we handle everything here
    # I THINK WE JUST NEED TO INTEGRATE UNDER Y, SO I DON'T THINK 
    # WE NEED THIS SPECIAL CASE, BUT CHECK.
    x0 <- x$x.scl
    f.at.x0 <- stats::approx(distances, y, xout = x0)$y 
  }
    

  # scaler <- x$g.x.scl * esw # esw = a length n vector, n = nrow(params) 

  # Did you know that 'scaler' is ESW?  At least for lines. Makes sense. 1/f(0) = ESW in 
  # the old formulas.
  # For smoothed distance functions, we extended the range of dist by approx 2, and scaler
  # is twice too big. i.e., esw for smu's is approx scaler / 2.
  
  # y <- y * scaler  # length(scalar) == nrow(y), so this works right
  
  attr(y, "x0") <- x0
  attr(y, "g.x.scl") <- x$g.x.scl
  # if(isSmooth){
  #   scaler <- scaler / 2
  # }
  # attr(y, "scaler") <- scaler

  
  return( y )  
}
