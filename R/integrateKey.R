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
integrateKey <- function(ml, key, likExpan, f0, plot = FALSE){
  
  nInts <- getOption("Rdistance_intEvalPts")
  intCoefs <- getOption("Rdistance_intCoefs")

  # nInts <- 501
  # intCoefs <- simpsonCoefs(nInts)
  
  # Make sure 0 is included in domain of key. We know g(0) and f(0)
  dObs <- c( setUnits(0,"m")
           , distances(ml) - ml$w.lo
           )
  dObs <- dropUnits(dObs)
  
  if( grepl("FALSE", likExpan) && !grepl("Gamma", likExpan) ){
    # line survey g(0) = 1; f(0) = ESW
    keyUni <- c(dropUnits(f0) , key)
  } else {
    # Point survey, and Gamma, g(0) = f(0) = 0
    keyUni <- c( 0, dropUnits(key) )
  }
  
  dObsUni <- dObs[!duplicated(dObs)]
  keyUni <- keyUni[!duplicated(dObs)]
  
  ord <- order(dObsUni)
  dObsUni <- dObsUni[ord]
  keyUni <- keyUni[ord]
  
  # Simpson integral -----
  # seqx = seq(ml$w.lo, ml$w.hi, length=nInts) 
  # d <- dropUnits(seqx - ml$w.lo) 
  # dx <- seqx[2] - seqx[1]  
  
  # for f(w.hi), use f(x) at largest dObs (rule = 2) 
  # fy <- stats::approx(dObsUni, keyUni, xout = d, rule = c(1,2) )$y
  # keyIntegral <- sum(fy*intCoefs)*dx/3 

  # R integrate ----
  F <- function(x, d, fd){
    approx(d, fd, xout = x, rule = c(1,2))$y
  }
  keyIntegralR <- integrate(f = F 
                          , lower = ml$w.lo
                          , upper = ml$w.hi
                          , d = dObsUni
                          , fd = keyUni)
  
  # Plot ----
  if( plot ){
    graphics::plot(dObsUni, keyUni, pch = 15,
                   xlab = "Distance",
                   ylab = "Likelihood"
                   )
    d <- dropUnits(seqx - ml$w.lo)
    fy <- stats::approx(dObsUni, keyUni, xout = d, rule = c(1,2) )$y
    points(d, fy, pch = 16, cex = .5, col="red")
    legend("topright"
          , legend = c("Observation", paste(nInts, "Lin. Approx Pts."))
          , pch = c(15,16)
          , col = c("black", "red"))
  }
  
  # cat(paste0("    Simpson integral of key ("
  #           , colorize(nInts)
  #           , " points) : "
  #           , colorize(keyIntegral)
  #           , " (should be ~1)\n"
  #           ))
  cat(paste0("    base::integrate key "
             , colorize(keyIntegralR$value)
             , " (+-"
             , colorize(formatC(keyIntegralR$abs.err, format="f", digits = 7))
             , ") (should be 1+-0.001)\n"
  ))
  
  invisible(1)
}