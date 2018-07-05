#' @title RdistanceControls
#' 
#' @description Returns a list of optimization controls used in 
#' \code{Rdistance} and provides a way to change them if needed. 
#' 
#' @param maxIters The maximum number of optimization 
#' iterations allowed.
#' 
#' @param evalMax The maximum number of objective function
#' evaluations allowed.
#' 
#' @param likeTol The maximum change in the likelihood 
#' (the objective) between
#' iterations that is tolerated during optimization.  
#' If the likelhood changes by less than this amount, 
#' optimization stops and a solution is declared. 
#' 
#' @param coefTol The maximum change in the model coefficients 
#' between
#' iterations that is tolerated during optimization.  
#' If the sum of squared coefficient differences changes 
#' by less than this amount between interations, 
#' optimization stops and a solution is declared. 
#'
#' 
#' @param optimizer The optimizer to use for optimization.  Results
#' vary between optimizers, so switching algorithms sometimes 
#' makes a poorly behaved distance function converge.  The valid 
#' values are \code{optim} for the `optim` function of package `optim`, 
#' or \code{nlminb} for the `nlminb` routine. 
#'
#' @param hessEps A vector of parameter distances used during 
#' computation of numeric second derivatives. Should have length 
#' 1 or the number of parameters in the model. See function 
#' \code{\link{secondDeriv}}. 
#' 
#' @return A list containing named components for each of the 
#' controls.  This list has the same components as this function 
#' has input parameters. 
#' 
#' @author Trent McDonald \email{tmcdonald@west-inc.com}
#' 
#' @examples 
#' # increase number of iterations
#' RdistanceControls(maxIters=2000)
#' 
#' # change optimizer and decrease tolerance
#' RdistanceControls(optimizer="optim", tol=1e-8) 
#' 
#' @export  

RdistanceControls <- function(optimizer="nlminb",
                              evalMax=2000,
                              maxIters=1000,
                              likeTol=1e-8,
                              coefTol=1.5e-8,
                              hessEps=1e-8){
  
  list(optimizer=optimizer,
       evalMax=evalMax,
       maxIters=maxIters,
       likeTol=likeTol,
       coefTol=coefTol,
       hessEps=hessEps
      )
  
}