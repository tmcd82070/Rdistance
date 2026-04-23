#' @title Distance function values at observations
#' 
#' @description
#' An internal prediction function to predict (compute) 
#' the values of distance functions at a set of observed values. 
#' Unlike `predDfuncs`, which evaluates distance
#' functions at EVERY input distance, this routine evaluates 
#' distance functions at only ONE distance. This is what's 
#' appropriate for likelihood computation.
#' This version allows for matrix inputs and 
#' uses matrix operations, and is thus faster than earlier
#' looping versions.
#' 
#' @inheritParams predict.dfunc 
#' @inheritParams predDfuncs
#' 
#' @details
#' 
#' Assuming `L` is the vector returned by this function, 
#' the negative log likelihood is `-sum(log(L / I), na.rm=T)`, 
#' where `I` is the integration constant, or 
#' area under the likelihood between 
#' `w.lo` and `w.hi`. 
#' Note that returned likelihood values for distances less 
#' than `w.lo` or greater than `w.hi` are `NA`; 
#' hence, `na.rm=TRUE` in the sum. 
#' 
#' @return A vector of distance function values, of length 
#' n = number of observed distances = length(distances(x)). 
#' Elements in `distances(x)` correspond, in order, 
#' to values in the returned vector.
#'  
#' @export
predLikelihood <- function(object
                         , params
                          ){
  
  d <- Rdistance::distances(object) # these should have the right units, don't check

  # Note: w.lo is subtracted in predDfuncs
  
  # This call to predDfuncs results in application of 
  # all params to all distances, i.e., an n X n matrix.  We only 
  # want the diagonal of this matrix for the likelihood. This 
  # method computes way too many values (all the off-diagonals)
  # but I cannot figure out a way to reduce computations without 
  # resorting to a loop and keeping the predict dfuncs methods.
  # This is something that could be modified in the future.
  
  # cat(colorize("in predLikelihood...\n", col="yellow", bg = "bgRed"))
  # cat(colorize(paste("dim(params) = ", paste(dim(params), collapse=","), "\n"),  col="yellow", bg = "bgRed"))
  # cat(colorize(paste("length(d) = ", paste(length(d), collapse=","), "\n"), col="yellow", bg = "bgRed"))
  
  y <- Rdistance::predDfuncs(object = object
                                , params = params
                                , distances = d
                                , isSmooth = is.smoothed(object))
  y <- diag(y)
 
  return( y )  
}
