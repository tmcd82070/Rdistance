#' @title Print abundance estimates
#' 
#' @description Print an object of class \code{c("abund","dfunc")} 
#' produced by \code{abundEstim}.
#' 
#' @param x An object output by \code{abundEstim}.  This is a distance 
#' function object augmented with abundance estimates, and has 
#' class \code{c("abund", "dfunc")}.
#'   
#' @param \dots Included for compatibility to other print methods.  
#' Ignored here.
#' 
#' @return 0 is invisibly returned
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}, 
#' \code{\link{summary.dfunc}}, \code{\link{print.dfunc}}, 
#' \code{\link{summary.abund}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist ~ 1 + offset(groupsize)
#'                   , detectionData=sparrowDetectionData)
#' 
#' # Estimate abundance given a detection function
#' # Note: a person should do more than R=20 bootstrap iterations
#' fit <- abundEstim(dfunc
#'                 , detectionData = sparrowDetectionData
#'                 , siteData = sparrowSiteData
#'                 , area = units::set_units(4105, "km^2")
#'                 , ci = NULL)
#' print(fit)
#' 
#' @keywords models
#' @export

print.abund <- function( x
                       , ... ){

  print.dfunc( x )
  cat("\n")
  hasCI <- all(!is.null(x$density.ci))
  
  # ---- Density printout ----
  mess <- c("Density in sampled area:")
  ptEst <- colorize( colorize(format(x$estimates$density)), col = "bold" )
  mess <- paste(mess, ptEst)
  cat(paste0(mess, "\n"))

  # ---- Abundance printout ----
  mess <- paste0( "Abundance in ", format(x$estimates$area), " study area:")
  ptEst <- colorize( colorize(format(x$estimates$abundance)), col = "bold" )
  mess <- paste(mess, ptEst)
  cat(paste0(mess, "\n"))

  invisible(0)
}
