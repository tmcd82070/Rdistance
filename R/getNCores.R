#' @title Set number of cores 
#' 
#' @description Set the number of cores for parallel operations. 
#' Also convert integer 'parallel' to TRUE-FALSE for ease. 
#' 
#' @inheritParams abundEstim 
#' 
#' @return A list with components $parallel and $cores. $parallel 
#' is logical, T for parallel operations, F o.w.  $cores is the 
#' number of cores to use during parallel operations.  $cores is 
#' integer in the range 1, 2, ..., max(Available cores). 
#' 
#' @details Input \code{parallel} <= 0 is converted to 1. 
#' Input \code{parallel} > maxCores is converted to maxCores.
#' If input \code{parallel} is numeric, is first converted to integer
#' by rounding down. 
#' 

getNCores <- function(parallel){
  maxCores <- parallel::detectCores()
  if( is.logical(parallel) ){
    if( parallel ){
      cores2Use <- maxCores - 1
    } else {
      cores2Use <- 1
    }
  } else {
    if( parallel < 1 ){
      warning(paste0("Number of requested cores must be >= 1. Found "
                    , parallel
                    , ". Running on 1 core.")
              , immediate. = TRUE)
    } else if( parallel > maxCores ){
      warning(paste0("Number of requested cores must be <= "
                    , maxCores
                    , ". Found "
                     , parallel
                     , ". Running on "
                     , maxCores
                     ," cores.")
              , immediate. = TRUE)
      
    }
    if( parallel >= 2 ){
      cores2Use <- min(as.integer(parallel), maxCores)
      parallel <- TRUE
    } else {
      cores2Use <- 1
      parallel <- FALSE
    }
  }
  # At this point, parallel is T-F, cores2use is 1,...,max
  
  list(parallel=parallel, cores = cores2Use)
}