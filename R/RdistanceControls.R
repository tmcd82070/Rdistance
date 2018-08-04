#' @title Control parameters for \code{Rdistance} optimization.
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
#' If the likelihood changes by less than this amount, 
#' optimization stops and a solution is declared. 
#' 
#' @param coefTol The maximum change in the model coefficients 
#' between
#' iterations that is tolerated during optimization.  
#' If the sum of squared coefficient differences changes 
#' by less than this amount between iterations, 
#' optimization stops and a solution is declared. 
#'
#' 
#' @param optimizer A string specifying the optimizer 
#' to use.  Results
#' vary between optimizers, so switching algorithms sometimes 
#' makes a poorly behaved distance function converge.  The valid 
#' values are "optim" which uses \code{optim::optim},
#' and "nlminb" which uses \code{stats:nlminb}.  The authors 
#' have had better luck with "nlminb" than "optim" and "nlminb" 
#' runs noticably faster.  Problems with solutions near parameter 
#' boundaries may require use of "optim".   
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
#' RdistanceControls(optimizer="optim", likeTol=1e-6) 
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