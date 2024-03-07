#' @title startLimits - Set distance function starting values and limits
#' 
#' @description 
#' Returns starting values and limits (boundaries) for the parameters of 
#'   distance functions.  This function is called by 
#'   other routines in \code{Rdistance}, and is not intended to 
#'   be called by the user.
#'
#' @param ml A list containing distance function components.  Distance
#' function components include "likelihood", "w.lo", "w.hi", "expansions", 
#' and the model frame, which contains the data. See 
#' \code{\link{parseModel}}
#' 
#' @details
#' 
#' \itemize{
#'  \item \code{\bold{ml$like}}: A string specifying the likelihood for the distance function.  
#' At present, its value must be one of:  
#'   "\bold{hazrate}" for the hazard rate likelihood; "\bold{halfnorm}" for the half 
#'   normal likelihood, "\bold{uniform}" for the uniform likelihood, 
#'   "\bold{negexp}" for the negative exponential likelihood, and 
#'   "\bold{Gamma}" for the gamma likelihood.
#'   
#' \item \code{\bold{ml$expan}} Number of expansion terms to include. Valid values are 0, 1, ..., 3.
#' 
#' \item \code{\bold{ml$w.lo}} Lower or left-truncation limit of the distances.  Normally, 0. 
#' Must have physical measurement units.
#' 
#' \item \code{\bold{ml$w.hi}} Upper or right-truncation limit of the distances. This is 
#' the maximum observable off-transect distance. Must have physical measurement units.
#' 
#' \item \code{\bold{ml$dist}} A vector of observed off-transect distances.  This vector is 
#' used to compute starting values and is only required for 
#' \code{like} = "Gamma" and "halfnorm". Must have physical measurement units.
#' 
#' \item \code{\bold{ml$mf}} The model frame containing distances and covariates, if any. 
#' 
#' \item \code{\bold{ml$transType}} Type of transects. Either "line" or "point". 
#' }
#' 
#' The number of parameters to be fitted is:
#'   \code{expan + 1 + 1*(like \%in\% c("hazrate", "uniform")) + (Num Covars)}.
#'   This is the length of vectors returned in the output list.
#'   
#' @return A list containing the following components
#'   \item{start}{Vector of starting values for parameters of the likelihood and expansion terms. }
#'   \item{lowlimit}{Vector of lower limits for the likelihood parameters and expansion terms.}
#'   \item{uplimit}{Vector of upper limits for the likelihood parameters and expansion terms.}
#'   \item{names}{Vector of names for the likelihood parameters and expansion terms.}
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
