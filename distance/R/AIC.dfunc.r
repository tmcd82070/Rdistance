AIC.dfunc <- function( obj, ..., k=2 ){

obj$loglik + k*length(coef(obj))

}
