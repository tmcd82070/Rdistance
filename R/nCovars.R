#' @title nCovars - Number of covariates
#' 
#' @description
#' Return number of covariates in a distance model
#' 
#' @param X The X matrix of covariates, or a vector. 
#' 
#' @details
#' The reason this routine is needed is that sometimes 
#' we pass one row of covariates to a likelihood function. 
#' If so, it may come in as a normal vector, not a matrix. 
#' If a normal vector, ncol(X) does not work. 
#' 
#' @return An integer scaler
#' 
#' # do not export
nCovars <- function(X){
  
  if( is.matrix(X) ){
    q <- ncol(X)
  } else {
    q <- length(X)
  }
  
  q
}