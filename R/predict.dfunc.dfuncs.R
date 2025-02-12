#' @title predict.dfunc.dfuncs - Predict distance functions
#' 
#' @description
#' An internal prediction function to predict a distance 
#' function.  This version allows for matrix inputs and 
#' uses matrix operations, and is thus faster than earlier
#' looping versions.
#' 
#' @inheritParams predict.dfunc
#' 
#' @param params A matrix of distance function parameters. 
#' Rows are observations, columns contain the set of parameters
#' (canonical and expansion) for each observation. 
#' 
#' @param isSmooth Logical.  TRUE if the distance function 
#' is smoothed (and hence has no parameters).
#' 
#' @return A matrix of distance functions.  Distance 
#' functions are in column.  Distances go across rows. 
#' i.e., can use matplot(d,return) to plot all distance 
#' functions. 
#' 
#' 
predict.dfunc.dfuncs <- function(x
                               , params
                               , distances
                               , isSmooth
                               
                               ){
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
    
    like <- utils::getFromNamespace(paste0( x$likelihood, ".like"), "Rdistance")    
    XIntOnly <- matrix(1, nrow = length(distances), ncol = 1)
    # XIntOnly <- diag(nD)
    nD <- length(distances)
    nP <- nrow(params)
    d <- matrix(distances - x$w.lo
              , nrow = nP
              , ncol = nD
              , byrow = TRUE)  
    y <- like(
             a = params
           , dist = d
           , covars = XIntOnly
    )
    y <- t(y$L.unscaled)

    if(x$expansions > 0){
      exp.terms <- Rdistance::expansionTerms(a = params
                                             , d = d[1,] # b/c w.lo subtracted
                                             , series = x$series
                                             , nexp = x$expansions
                                             , w = x$w.hi - x$w.lo)
      y <- y * exp.terms
      
      # without monotonicity restraints, function can go negative, 
      # especially in a gap between datapoints. Don't want this in distance
      # sampling and screws up the convergence. In future, could
      # apply monotonicity constraint here.
      y[ !is.na(y) & (y <= 0) ] <- getOption("Rdistance_zero")
      
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
    
    # Likelihood functions return g(x). This is what we want, 
    # except that if there are expansions, g(0) != 1
    # Gotta loop here because like() assumes a = params is a vector, 
    # not a matrix (could re-write likelihoods to accept matricies of parameters).

    d <- matrix(x0 - x$w.lo
              , nrow = length(x0)
              , ncol = 1)
    XIntOnly <- matrix(1, nrow = nrow(d), ncol = 1)
    
    # here, length(x0) == nrow(params) == nrow(d) 
    f.at.x0 <- like(a = params
                  , dist = d
                  , covars = XIntOnly)    
    f.at.x0 <- t(f.at.x0$L.unscaled)

    if(x$expansions > 0){
      exp.terms <- Rdistance::expansionTerms(a = params
                                             , d = d
                                             , series = x$series
                                             , nexp = x$expansions
                                             , w = x$w.hi - x$w.lo)
      f.at.x0 <- f.at.x0 * exp.terms
      f.at.x0[ !is.na(f.at.x0) & (f.at.x0 <= 0) ] <- getOption("Rdistance_zero")
    }
    
    
  } else {
    # Case: smoothed distance function.  No covars. 
    # This is so simple we handle everything here
    # I THINK WE JUST NEED TO INTEGRATE UNDER Y, SO I DON'T THINK 
    # WE NEED THIS SPECIAL CASE, BUT CHECK.
    x0 <- x$x.scl
    f.at.x0 <- stats::approx(distances, y, xout = x0)$y 
  }
  
  # length(f.at.x0) must equal ncol(y)
  # use t() twice b/c I assume R's internal expansion of f.at.x0
  # to apply to all columns of y is more efficient than expanding 
  # f.at.x0 to a full matrix
  y <- x$g.x.scl * t(t(y) / drop(f.at.x0))
  # the above works only if x$g.x.scl is a scalar; 
  # otherwise, we'd need to
  # evaluate f(x0) for every x0, then multiply.
  
  # Did you know that 'scaler' is ESW?  At least for lines. 
  # Makes sense. 1/f(0) = ESW in 
  # the old formulas.
  # For smoothed distance functions, we extended the range of dist by approx 2, and scaler
  # is twice too big. i.e., esw for smu's is approx scaler / 2.
  
  # y <- y * scaler  # length(scalar) == nrow(y), so this works right
  
  attr(y, "distances") <- distances
  attr(y, "x0") <- x0
  attr(y, "g.x.scl") <- x$g.x.scl
  # if(isSmooth){
  #   scaler <- scaler / 2
  # }
  # attr(y, "scaler") <- scaler
  
  return( y )  
}