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

insertOneStepBreaks <- function(obj = x
                              , newData = newdata
                              , xseq){
  
  parms <- predict(object = obj
                 , newdata = newData
                 , type = "parameters")
  
  epsilon <- matrix(c(-1,1) * 100 * .Machine$double.neg.eps
                  , nrow = nrow(parms)
                  , ncol = 2
                  , byrow = TRUE)
  breaks <- parms[,1] + epsilon
  breaks <- units::set_units(c(t(breaks)), units(xseq), mode="standard")
  xseq <- sort(c(xseq, breaks))
  xseq
}