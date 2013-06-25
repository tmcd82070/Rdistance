AIC.dfunc <- function( object, ..., k=2, n=length(object$dist)  ){

p <- length(coef(object))

object$loglik + k*p + (2*p*(p+1)) / (n - p - 1)

}
