#' @title Compute and print distance function integration
#' 
#' @description Check several integration characteristics, 
#' and report them to screen.  This was designed to print 
#' info when user asks for higher verbose level. The scaled
#' distance function should integrate to 1.0 every iteration of 
#' maximization, and this function checks that. 
#' 
#' @param ml The fitted object or model list
#' 
#' @param key The scaled distance function.  
#' 
#' @param likExpan A string specifying the current estimation case.
#' Format of the string is <likelihood>_<num expansions>_<points?>. 
#' E.g., "halfnorm_0_FALSE" specifies halfnormal, no expansions, and 
#' line transect data. 
#' 
#' @param f0 The value of f(0) when integrating line transects. 
#' This should be the ESW for the case. 
#' 
#' 
#' @return Nothing.  Prints information on integrals to the screen.
#' 
integrateKey <- function(ml, key, likExpan, f0){
  
  nInts <- getOption("Rdistance_intEvalPts") 
  intCoefs <- getOption("Rdistance_intCoefs")    
  
  # Make sure 0 is included in domain of key. We know g(0) and f(0)
  dObs <- c( units::set_units(0,"m")
           , distances(ml) - ml$w.lo
           )
  dObs <- units::set_units(dObs, NULL)
  
  if( grepl("FALSE", likExpan) ){
    # line survey g(0) = 1; f(0) = ESW
    keyUni <- c(units::set_units(f0, NULL) , key)
  } else {
    # Point survey g(0) = f(0) = 0
    keyUni <- c( 0, units::set_units(key, NULL) )
  }
  
  dObsUni <- dObs[!duplicated(dObs)]
  keyUni <- keyUni[!duplicated(dObs)]
  
  seqx = seq(ml$w.lo, ml$w.hi, length=nInts) 
  d <- units::set_units(seqx - ml$w.lo, NULL) 
  dx <- seqx[2] - seqx[1]  
  
  # for f(w.hi), use f(x) at largest dObs (rule = 2) 
  fy <- stats::approx(dObsUni, keyUni, xout = d, rule = c(1,2) )$y
  keyIntegral <- sum(fy*intCoefs)*dx/3 

  cat(paste0("  Integral of key ("
            , colorize(nInts)
            , " points) : "
            , colorize(keyIntegral)
            , " (should be ~1)\n"
            ))

  invisible(1)
}