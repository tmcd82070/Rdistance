#' @title Predict method for dfunc objects
#' 
#' @description Predict likelihood parameters or inflation 
#' factors for distance function objects.
#' 
#' @param object An estimated dfunc object.  See \code{\link{dfuncEstim}}. 
#' 
#' @param newdata A data frame containing new values of 
#' the covariates at which predictions are to be computed. 
#' 
#' @param type The type of predictions desired. Currently, only 
#' type = "parameters" is implemented and returns 
#' parameters of the likelihood function.  
#' 
#' @return A vector of predicted parameters or inflation factors.
#' 
#' @author Trent McDonald
#' 
#' @export
#' 
#' 

# Extra Roxygen comments when we get around implmenting other types of
# predictions 
# Type = "inflation" predicts the inflation factor for all
# observations.  Inflation factors use likelihood parameters to compute
# effective sampling distances (ESW or ESR) and inverts them.


 
predict.dfunc <- function(object, newdata, 
          type = c("parameters"), ...) 
{
  if (!inherits(object, "dfunc")) 
    stop("object is not a dfunc")
  
  hasCovars <- !is.null(object$covars)
  
  if (missing(newdata) || is.null(newdata)) {
    n <- length(object$dist)
  } else {
    n <- nrow(newdata)
  }
  
  if(hasCovars){
    # X is the covariate matrix for predictions 
    if (missing(newdata) || is.null(newdata)) {
      X <- object$covars
    } else {
      Terms <- terms(as.formula(object$call[["formula"]]))
      Terms <- delete.response(Terms)
      m <- model.frame(Terms, newdata)
      X <- model.matrix(Terms, m, contrasts.arg = attr(object$covars,"contrasts"))
    }
    
    BETA <- coef(object)
    beta <- BETA[1:ncol(X)]   # could be extra parameters tacked on. e.g., knee for uniform
    params <- X %*% beta
    params <- exp(params)  # All link functions are exp...thus far
    if(ncol(X)<length(BETA)){
      extraParams <- matrix(BETA[(ncol(X)+1):length(BETA)], n, length(BETA)-ncol(X), byrow=TRUE)
      params <- cbind(params, extraParams)
    }
  } else {
    params <- coef(object) 
    params <- matrix(params, nrow=n, ncol=length(params), byrow=TRUE)
  }  

  dimnames(params)[[2]] <- likeParamNames(object$like.form)
  
  # Implement different types of predictions here. 
  # type <- match.arg(type)
  # if(type == "inflation"){
  #   params <- effectiveDistance()
  # }

  params
}