#' @title Insert oneStep Likelihood breaks
#' 
#' @description Compute break points in a onestep likelihood
#' and insert them into a sequence of distances.  The idea 
#' is to insert a point just left and just right of the breaks 
#' so that they plot as vertical lines. 
#' 
#' @param obj A fitted Rdistance model object
#' 
#' @param newData A data frame containing covariate values to 
#' use in prediction. 
#' 
#' @param xseq A vector of distances into which the break points 
#' are inserted. 
#' 
#' @return A vector like \code{xseq}, but with the break points 
#' inserted. 
#' 
#' 

# Do not export

insertOneStepBreaks <- function(obj 
                              , newData 
                              , xseq){
  
  parms <- stats::predict(object = obj
                 , newdata = newData
                 , type = "parameters")[, 1]
  parms <- units::set_units(parms, units(xseq), mode = "standard")
  parms <- parms + obj$w.lo # matrix + scalar
  
  epsilon <- matrix(c(-1,1) * 100 * .Machine$double.neg.eps
                  , nrow = length(parms)
                  , ncol = 2
                  , byrow = TRUE)
  epsilon <- units::set_units(epsilon, units(xseq), mode = "standard")
  breaks <- parms + epsilon # vector length n + n X 2 matrix
  breaks <- c(t(breaks))
  xseq <- sort(c(xseq, breaks))
  xseq
}