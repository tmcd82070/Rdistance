#' @title predict.dfunc.likelihood - Distance function values at observations
#' 
#' @description
#' An internal prediction function to predict (compute) 
#' the values of distance functions at a set of observed values. 
#' Unlike \code{predict.dfunc.dfuncs}, which evaluates distance
#' functions at EVERY input distance, this routine evaluates 
#' distance functions at only ONE distance. This is what's 
#' appropriate for likelihood computation.
#' This version allows for matrix inputs and 
#' uses matrix operations, and is thus faster than earlier
#' looping versions.
#' 
#' @inheritParams predict.dfunc 
#' @inheritParams predict.dfunc.dfuncs
#' 
#' 
#' @return A vector of distance function values, of length 
#' n = number of observed distances = length(distances(x)). 
#' Each observation is associated with one element
#' 
#' 
predict.dfunc.likelihood <- function(x
                               , params
                               ){
  
  d <- Rdistance::distances(x) # these should have the right units, don't check

  # Note: w.lo is subtracted in predict.dfunc.dfuncs
  
  # This call to predict.dfunc.dfuncs results in application of 
  # all params to all distances, i.e., an n X n matrix.  We only 
  # want the diagonal of this matrix for the likelihood. This 
  # method computes way too many values (all the off-diagonals)
  # but I cannot figure out a way to reduce computations without 
  # resorting to a loop and keeping the predict dfuncs methods.
  # This is something that could be modified in the future.
  
  y <- Rdistance:::predict.dfunc.dfuncs(x = x
                                , params = params
                                , distances = d
                                , isSmooth = is.smoothed(x))
  y <- diag(y)
 
  return( y )  
}