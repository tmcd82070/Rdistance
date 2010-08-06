AIC.dfunc <- function( obj, k=2, n=length(obj$dist), ...  ){

p <- length(coef(obj))

obj$loglik + k*p + (2*p*(p+1)) / (n - p - 1)

}
