#' @title Likelihood parameter names
#' 
#' @description Returns names of the likelihood parameters. This is 
#' a helper function and is not
#' necessary for estimation.  It is nice to label some 
#' parameters with descriptive
#' names like "sigma" or "k", depending on the likelihood.
#' 
#' @param like.form A text string naming the form of the likelihood. 
#' An error is thrown if the likelihood is unknown.
#' 
#' @return A vector of parameter names for the likelihood
#' 
#' @export
#' 
likeParamNames <- function(like.form){
  
  switch(like.form
       , halfnorm = c("Sigma")
       , hazrate = c("Sigma", "k")
       , uniform = c("Threshold", "Knee")
       , negexp = c("Alpha")
       , oneStep = c("Theta", "p")
       , Gamma = c("Scale", "Shape")
       , triangle = c("Theta", "p")
       , huber = c("Theta1", "Theta2", "p")
       , {    
           stop(paste("Unknown likelihood. Found", like.form))
         }
  )
  
}