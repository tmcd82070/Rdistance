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
#' runs noticeably faster.  Problems with solutions near parameter 
#' boundaries may require use of "optim".   
#'
#' @param hessEps A vector of parameter distances used during 
#' computation of numeric second derivatives. Should have length 
#' 1 or the number of parameters in the model. See function 
#' \code{\link{secondDeriv}}. 
#' 
#' @param requireUnits A logical specifying whether measurement 
#' units are required on distances and areas.  If TRUE, 
#' measurement units are required on off-transect and radial 
#' distances in the input data frame.  Likewise, measurement 
#' units are required on transect length and study area size. 
#' Assign units with statement like \code{units(detectionDf$dist) <- "m"}
#' or \code{units(df$transectDf) <- "km"}.  Measurement units do not 
#' need to be the same.  All units are converted appropriately during 
#' internal computations.  \code{Rdistance} recognizes 
#' units listed in \code{units::valid_udunits()}. 
#' 
#' @param maxBSFailPropForWarning The proportion of bootstrap 
#' iterations that can fail without a warning. If the proportion 
#' of bootstrap iterations that did not converge exceeds this 
#' parameter, a warning about the validity of CI's is issued in 
#' the print method for
#' abundance objects. 
#' 
#' @param contrasts A list, whose entries are values 
#' (numeric matrices, functions or character strings naming functions) 
#' to be used as replacement values for the default contrasts function 
#' and whose names are the names of columns of data containing factors.
#' 
#' There are several ways to change the contrasts used for factors 
#' in Rdistance because all methods used in linear models (\code{lm})
#' work.  To summarize contrast methods in R, if this parameter is NULL, Rdistance uses 
#' the global contrasts specified in 
#' \code{options()}.  To change the global contrasts, use a statement
#' like \code{options(contrasts = c(unordered = "contr.SAS", 
#' ordered = "contr.poly"))}.
#' One can also set contrasts for a factor using \code{contrasts(a)} 
#' (e.g., \code{contrasts(a) <- "contr.sum"}) 
#' Lastly, one can set this parameter to a  
#' list that explicitely states the non-global contrasts to use for 
#' which factors in 
#' the Rdistance model.  For example, \code{list(a = "contr.helmert")} 
#' will use Helmert contrasts for \code{a} 
#' and the global contrast option for all other factors. The 
#' built-in R contrast functions are "contr.treatment", "contr.helmert", 
#' "contr.SAS", "contr.sum", and "contr.poly".  
#' 
#' @return A list containing named components for each of the 
#' controls.  This list has the same components as this function 
#' has input parameters. 
#' 
#' @examples 
#' # increase number of iterations
#' RdistanceControls(maxIters=2000)
#' 
#' # change optimizer and decrease tolerance
#' RdistanceControls(optimizer="optim", likeTol=1e-6) 
#' 
#' @export  

RdistanceControls <- function(optimizer = "nlminb",
                              evalMax = 2000,
                              maxIters = 1000,
                              likeTol = 1e-8,
                              coefTol = 1.5e-8,
                              hessEps = 1e-8,
                              requireUnits = TRUE,
                              maxBSFailPropForWarning = 0.2,
                              contrasts = NULL){
  
  list(optimizer = optimizer,
       evalMax = evalMax,
       maxIters = maxIters,
       likeTol = likeTol,
       coefTol = coefTol,
       hessEps = hessEps,
       requireUnits = requireUnits,
       maxBSFailPropForWarning = maxBSFailPropForWarning,
       contrasts = contrasts
      )
  
}
