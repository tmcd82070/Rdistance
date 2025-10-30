#' @title Check number of numeric integration intervals
#' 
#' @description
#' Check that number of integration intervals is odd and 
#' sufficiently large. Plus, compute and store the Simpson's Composite
#' rule coefficients. 
#' 
#' @param nEvalPts An integer to check.
#' 
#' @return The first element of nEvalPts is returned if it is acceptable.
#' If nEvalPts is not acceptable, an error is thrown. Simpson's composite
#' coefficients are store in options()
#' 
checkNEvalPts <- function(nEvalPts){
  
  if( length(nEvalPts) > 1){
    warning(paste0("Only first element of option 'Rdistance_intEvalPts' used."))  
    nEvalPts <- nEvalPts[1]
  }
  
  if( (nEvalPts %% 2) == 0 ) {
    stop(paste0("Option 'Rdistance_intEvalPts' must be odd for numerical integration. Found ", 
                nEvalPts
                , ". Reset with statement like 'options(Rdistance_intEvalPts = "
                , nEvalPts + 1
                , ")'"))
  }
  
  if( nEvalPts < 29 ){
    warning( paste0("Number of integration intervals is low. "
                    , "Found ", nEvalPts
                    , ". Recommend options('Rdistance_intEvalPts' = 101) or higher. "
                    , " Speed is not dramatically faster for low values "
                    , "but accuracy can be reduced."
                    ))
  }
  
  # Store Simpson coefficients in options() to speed calculations a little
  intCoefs <- simpsonCoefs( nEvalPts )
  options("Rdistance_intCoefs" = intCoefs)
  
  invisible(nEvalPts)
}
