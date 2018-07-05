#' @title secondDeriv
#' 
#' @description Computes numeric 2nd derivatives (hessian) of an 
#' arbitrary multidimensional function at a particular location.
#' 
#' @param x The location (a vector) where the second derivatives 
#' of \code{FUN} are desired.
#' 
#' @param FUN An R function for which the second derivatives are 
#' sought.  
#' This must be a function of the form FUN <- function(x, ...){...}
#' where x is a vector of variable parameters to FUN at which 
#' to evaluate the 2nd derivative, 
#' and ... are additional paramters needed to evaluate the function.
#' FUN must return a single value (scalar), the height of the 
#' surface above \code{x}, i.e., FUN evaluated at x.  
#' 
#' @param eps A vector of small distances to add to \code{x} 
#' when evaluating derivatives.   This is the 'delta x' or eps of 
#' the numerical derivatives. Default value 1e-8 for all 
#' dimensions.  If length of \code{eps} is less than length of \code{x},
#' \code{eps} is replicated to the length of \code{x}.  
#' 
#' @details This function uses the "5-point" numeric second derivative 
#' method advocated in numerous numerical recipie texts.  During computation
#' of the 2nd derivative, FUN nust be 
#' capable of being evaluated at locations within a hyper-elipsoid 
#' with cardinal radii 2*\code{x}*(eps)^0.25, or 0.02*\code{x} at the 
#' default value for \code{eps}.   
#' 
#' A handy way to use this function is to call an optimization routine 
#' like \code{nlminb} with FUN, then call this function with the 
#' optimized values (solution) and FUN.  This will yeild the hessian 
#' at the solution rather than the hessian at the previous step of the 
#' optimization. 
#' 
#' @author Trent McDonald
#' 
#' @examples 
#' 
#'func <- function(x){-x*x} # second derivative should be -2
#'F.2nd.deriv(0,func)
#'F.2nd.deriv(3,func)
#'
#'func <- function(x){3 + 5*x^2 + 2*x^3} # second derivative should be 10+12x
#'F.2nd.deriv(0,func)
#'F.2nd.deriv(2,func)
#'
#'func <- function(x){x[1]^2 + 5*x[2]^2} # should be rbind(c(2,0),c(0,10))
#'F.2nd.deriv(c(1,1),func)
#'
#' @export

secondDeriv <- function(x, FUN, eps=1e-8, ...){
  d <- length(x)   # number of dimensions
  if(d > length(eps)){
    eps <- rep(eps,ceiling( d/length(eps) ))[1:d]
  } else if( length(eps) > d){
    eps <- eps[1:d]
  }
  FUN <- match.fun(FUN)
  hess <- matrix(0, nrow=d, ncol=d)
  h <- ifelse(x==0, eps^0.25, (eps^(0.25))*x )
  for(i in 1:d){
    ei <- rep(0,d)
    ei[i] <- 1
    # compute diagonal element
    hess[i,i] <- (-FUN(x+2*h*ei, ...) + 
                   16*FUN(x+h*ei, ...) - 
                   30*FUN(x, ...) +
                   16*FUN(x-h*ei, ...) - 
                   FUN(x-2*h*ei, ...)) / (12*h[i]*h[i])
    if((i+1) <= d){
      for(j in (i+1):d){
        ej <- rep(0,d)
        ej[j] <- 1
        # compute off diagonal element
        hess[i,j] <- (FUN(x+h*ei+h*ej, ...) - 
                        FUN(x+h*ei-h*ej, ...) -
                        FUN(x-h*ei+h*ej, ...) + 
                        FUN(x-h*ei-h*ej, ...)) / (4*h[i]*h[j])
        # Assume symetric
        hess[j,i] <- hess[i,j]
      }
    }
  }
  hess
}
