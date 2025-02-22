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
#' @return A matrix of distance function values, of size 
#' length(distances) X nrow(params).  Each row of params
#' is associated with a column, i.e., a different distance 
#' function.  Distances are associated with rows, 
#' i.e., use matplot(d,return) to plot values on separate distance 
#' functions specified by rows of params.
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
    d <- distances - x$w.lo
    XIntOnly <- matrix(1, nrow = length(d), ncol = 1)
    y <- like(
             a = params
           , dist = d
           , covars = XIntOnly
    )
    y <- y$L.unscaled # (nXk) = (length(d) X nrow(params))

    if(x$expansions > 0){
      # expansion terms are always constant across distances
      # Hence, length of params does not matter, return n = length(d) vector
      exp.terms <- Rdistance::expansionTerms(a = params
                                             , d = d 
                                             , series = x$series
                                             , nexp = x$expansions
                                             , w = x$w.hi - x$w.lo)
      y <- y * exp.terms # (nXk) * (nXk)
      
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
    # Gamma case. Only x.scl allowed is 'max'. 
    # If there are covariates, we need different x.scl for 
    # every distance function.  
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
      maximize.g(fit, covars = covRow)
    }
    
    x0 <- apply(X = X
                , MARGIN = 1
                , FUN = maximize.g.reparam
                , fit = x
    )
    # x0 is a distance, needs units
    x0 <- units::set_units(x0, x$outputUnits, mode = "standard")
    
  } else if( is.character(x$x.scl) && (x$x.scl == "max") ){
    # Technically, we could do this. Just like Gamma, we could 
    # estimate x.max for every distance function in y
    # something like...
    # full.df <- predict.dfunc(x, type="dfuncs", newdata = newdata)
    # maxPos <- apply(full.df, 2, which.max)
    # x0 <- attr(full.df, "distances")[maxPos]
    
    stop(paste0("x.scl = 'max' not currently implemented for non-Gamma likelihoods."))
  } else if( !isSmooth ){
    # Case:  All likelihoods except Gamma
    x0 <- x$x.scl
  } 
  
  # Now that we know x0 (either a scaler or a vector), compute f(x0)
  
  if( !isSmooth ){
    
    # Likelihood functions return g(x). This is what we want, 
    # except that if there are expansions, g(0) != 1

    d <- x0 - x$w.lo
    XIntOnly <- matrix(1, nrow = length(d), ncol = 1)
    
    # here, length(x0) == nrow(d) == 1
    f.at.x0 <- like(a = params
                  , dist = d
                  , covars = XIntOnly)    
    f.at.x0 <- f.at.x0$L.unscaled  # (1Xk)
    
    if(x$expansions > 0){
      exp.terms <- Rdistance::expansionTerms(a = params
                                             , d = d
                                             , series = x$series
                                             , nexp = x$expansions
                                             , w = x$w.hi - x$w.lo)
      f.at.x0 <- f.at.x0 * exp.terms # (1Xk) * (1)
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
  
  # ncol(f.at.x0) must equal ncol(y), or error here
  f.at.x0 <- matrix(f.at.x0, nrow(y), length(f.at.x0), byrow = TRUE)
  y <- x$g.x.scl * (y / f.at.x0)
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