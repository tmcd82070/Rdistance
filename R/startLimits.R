#' @title startLimits - Distance function starting values and limits
#' 
#' @description 
#' Returns starting values and limits (boundaries) for the parameters of 
#'   distance functions.  This function is called by 
#'   other routines in \code{Rdistance}, and is not intended to 
#'   be called by the user. Replace this function in the global 
#'   environment to change boundaries and starting values. 
#'
#' @param ml Either a Rdistance 'model frame' or an Rdistance 
#' 'fitted object'.  Both are of class "dfunc". 
#' Rdistance 'model frames' are lists containing components 
#' necessary to estimate a distance function, but no estimates. 
#' Rdistance 'model frames' are typically
#' produced by calls to \code{\link{parseModel}}. 
#' Rdistance 'fitted objects'
#' are typically produced by calls to \code{\link{dfuncEstim}}.
#' 'Fitted objects' are 'model frames'
#' with additional components such as the parameters estimates, 
#' log likelihood value, convergence information, and the variance-
#' covariance matrix of the parameters. 
#' 
#' 
#'   
#' @return A list containing the following components
#'   \item{start}{Vector of starting values for parameters of the likelihood and expansion terms. }
#'   \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
#'   \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
#'   \item{names}{Vector of names for the likelihood parameters and expansion terms.}
#'   
#' The length of each vector in the return is:
#'   \code{(Num expansions) + 1 + 1*(like \%in\% c("hazrate")) + (Num Covars)}.
#'  
#' @seealso \code{\link{dfuncEstim}}
#' @examples 
#'   data(sparrowDetectionData)
#'   sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#'   
#'   # Half-normal start limits
#'   modList <- parseModel(
#'        data = sparrowDf
#'      , formula = dist ~ 1
#'      , likelihood = "halfnorm"
#'   )
#'   startLimits(modList)
#'   
#'   # Half-normal with expansions
#'   modList <- parseModel(
#'        data = sparrowDf
#'      , formula = dist ~ 1
#'      , likelihood = "halfnorm"
#'      , expansions = 3
#'   )
#'   startLimits(modList)
#'   
#'   # Hazard rate start limits
#'   modList$likelihood <- "hazrate"
#'   startLimits(modList)
#'   
#'   # Neg exp start limits
#'   modList$likelihood <- "negexp"
#'   startLimits(modList)
#'   
#'   # Gamma start limits
#'   modList$likelihood <- "Gamma"
#'   startLimits(modList)
#'   
#'   # Logistic start limits
#'   modList$likelihood <- "logistic"
#'   startLimits(modList)
#'   
#' @keywords models
#' 
#' @export
startLimits <- function( ml ){

  # All start limit functions handled by likelihood-specific function
  like.sl <- match.fun( paste(ml$like, ".start.limits", sep="") )
  ans <- like.sl(ml)

  ans
  
}
