#' @title Differentiable likelihoods in Rdistance
#' 
#' @description Return a character vector of 
#' likelihoods which are differentiable and hence
#' the second derivative method can be used to estimate
#' variance-covariance.  Any likelihoods not in this list
#' must use bootstrapping. This vector is polled in 
#' a few Rdistance routines, notably \code{parseModel}
#' and \code{varcovarEstim}.
#' 
#' @return A character vector of differentiable 
#' likelihoods. 

# Do not export

differentiableLikelihoods<-function(){
  
  c("halfnorm", "hazrate", "negexp")
  
}