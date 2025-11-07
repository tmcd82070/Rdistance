#' @title Rdistance optimization control parameters.
#' 
#' @aliases control controls RdistanceControls
#' 
#' @concept control optimization
#' 
#' @description Optimization control parameters 
#' are set by calls to \code{options()} (see examples). 
#' Optimization parameters used in 
#' \code{Rdistance} are the following:  
#' 
#' \itemize{
#'   \item \code{Rdistance_maxIters}: The maximum number of optimization 
#' iterations allowed.
#' 
#'   \item \code{Rdistance_evalMax}: The maximum number of objective function
#' evaluations allowed.
#' 
#'   \item \code{Rdistance_likeTol}: Minimum change in the likelihood 
#' between iterations required optimization to continue.  
#' If the likelihood changes by less than this amount, 
#' optimization stops and a solution is declared. Iteration 
#' continues when likelihood changes exceed this value.
#' 
#'   \item \code{Rdistance_coefTol}: Minimum change in model coefficients 
#' between iterations for optimization to continue.  
#' If the sum of squared coefficient differences changes 
#' by less than this amount between iterations, 
#' optimization stops and a solution is declared. 
#'
#'   \item \code{Rdistance_optimizer}: A string specifying the optimizer 
#' to use.  Results can vary among optimizers, so 
#' switching algorithms sometimes makes a poorly 
#' behaved distance function converge, particularly when 
#' parameters are near their boundaries.  Valid 
#' values are: 
#' \itemize{
#'    \item "optim": Uses \code{optim::optim}, method "L-BFGS-B", 
#'    a finite-difference gradient based approach.
#'    \item "nlminb": Uses \code{stats:nlminb}, a finite-difference
#'    gradient based approach. 
#'    \item "hookeJeeves": Uses \code{dfoptim::hjkb}, a 
#'    derivative-free approach for continuous and discontinuous 
#'    likelihoods.
#' }
#' The authors have had better luck with "nlminb" 
#' when likelihoods are differentiable (i.e. smooth). 
#' "optim" seems to perform better when solutions are near, 
#' but not on,parameter boundaries. "hookeJeeves" works well in 
#' all cases but is slightly slower than "nlminb".    
#'
#'   \item \code{Rdistance_hessEps}: A vector of parameter distances used during 
#' computation of numeric second derivatives. These distances control
#' and determine variance estimates, and they may need revision when 
#' the maximum likelihood solution is near a parameter boundary. 
#' Should have length 
#' 1 or the number of parameters in the model. See function 
#' \code{\link{secondDeriv}} for further details. 
#' 
#'   \item \code{Rdistance_trace}: Integer scalar for the level 
#'   of information printed to the console by the optimization 
#'   routine during maximization of the likelihood. All optimizer 
#'   routines interpret a value of 0 as 'do not print any information'
#'   or silent.  Higher values produce more information.  The 
#'   information produced varies among optimization routines. 
#'  
#'   \item \code{Rdistance_requireUnits}: A logical specifying whether measurement 
#' units are required on distances and areas.  If TRUE, 
#' measurement units are required on off-transect and radial 
#' distances in the input data frame.  Likewise, measurement 
#' units are required on truncation distances, scale location, 
#' transect lengths, and study area size. If FALSE, no units are 
#' required and input values are used as is.  The FALSE options is 
#' provided for rare cases when \code{Rdistance} functions are called
#' from other functions and the calling functions do not accommodate 
#' units.
#' 
#' Assign units with statement like \code{units(detectionDf$dist) <- "m"}
#' or \code{setUnits(w.hi, "km")} or \code{w.hi <- 150 \%#\% "m"} or 
#' \code{w.hi <- 150 \%m\%.}.  
#' Measurement units of 
#' the various physical quantities need not 
#' be equal because appropriate conversions occur internally.
#' An error is thrown if differing units are not compatible.  
#' For example, "m" (meters) cannot be converted into "ha" (hectares),
#' but "acres" can be converted into "ha".
#' \code{Rdistance} recognizes units listed in \code{units::}\code{\link[units]{valid_udunits}}. 
#' 
#'   \item \code{Rdistance_maxBSFailPropForWarning}: The proportion of bootstrap 
#' iterations that can fail without a warning. If the proportion 
#' of non-convergent bootstrap iterations exceeds this 
#' parameter, a warning about the validity of CI's is issued in 
#' the abundance print method. 
#' 
#' 
#' }
#' 
#' @examples 
#' # increase number of iterations
#' options(Rdistance_maxIters=2000)
#' 
#' # change optimizer and decrease tolerance
#' options(list(Rdistance_optimizer="optim", Rdistance_likeTol=1e-6)) 
#' 
#' @name RdistanceControls
NULL
