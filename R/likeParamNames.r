#' @title Likelihood parameter names
#' 
#' @description Returns names of the likelihood paramters. This is 
#' a helper function and is not
#' necessary for estimation.  It is a nice to label some 
#' outputs in \code{Rdistance} with 
#' parameter names like "sigma" or "knee", depending on the likelihood, 
#' and this routine provides a way to do that. 
#' 
#' @param like.form A text string naming the form of the likelihood. 
#' 
#' @return A vector of parameter names for that likelihood
#' 
#' @details For user defined functions, ensure that the 
#' user defined start-limits function named <likelihood>.start.limits
#' can be evaluated on a distance of 1, can accept 0 expansions, 
#' a low limit of 0 
#' a high limit of 1, and that it returns the parameter names as 
#' the \code{$names} component of the result. That is, the 
#' code that returns user-defined parameter names is, 
#' \code{fn <- match.fun( paste0(like.form, ".start.limits"));
#' ans <- fn(1, 0, 0, 1);
#' ans$names}
#' 
#' @author Trent McDonald
#' 
#' @export
#' 
likeParamNames <- function(like.form){
  
  switch(like.form,
         halfnorm = c("Sigma"),
         hazrate = c("Sigma", "Beta"),
         uniform = c("Threshold", "Knee"), 
         negexp = c("Beta"),
         Gamma = c("Shape", "Scale"),
         {    
           #   Assume this is a user-defined likelihood
           fn <- match.fun( paste(like.form, ".start.limits", sep="") )
           ans <- fn(1, 0, 0, 1)
           ans$names
         }
  )
  
}