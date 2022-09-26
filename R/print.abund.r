#' @title Print abundance estimates
#' 
#' @description Print an object of class \code{c("abund","dfunc")} 
#' that is output by \code{abundEstim}.
#' 
#' @param x An object output by \code{abundEstim}.  This is a distance function object that 
#'   also contains abundance estimates, and has class \code{c("abund", "dfunc")}.
#'   
#' @param criterion A string specifying the criterion to print.
#' Must be one of "AICc" (the default), 
#' "AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas. 
#' 
#' @param maxBSFailPropForWarning The proportion of bootstrap 
#' iterations that can fail without a warning. If the proportion 
#' of bootstrap iterations that did not converge exceeds this 
#' parameter, a warning about the validity of CI's is issued and 
#' a diagnostic message printed.  Increasing this to a number greater 
#' than 1 will kill the warning, but ignoring a large number of non-convergent
#' bootstrap iterations may be a bad idea (i.e., validity of the CI is 
#' questionable). 
#' 
#' @param \dots Included for compatibility to other print methods.  Ignored here.
#' @details The default print method for class 'dfunc' is called, then the abundance estimates 
#'   contained in \code{obj} are printed.
#' @return No value is returned.
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData,
#'                     likelihood="halfnorm", w.hi=100, pointSurvey=FALSE)
#' 
#' # Estimate abundance given a detection function
#' # Note, area=10000 converts to density per hectare (for distances measured in meters)
#' # Note, a person should do more than R=20 iterations
#' fit <- abundEstim(dfunc, detectionData=sparrowDetectionData,
#'                   siteData=sparrowSiteData, area=10000, R=20, ci=0.95,
#'                   plot.bs=TRUE, bySite=FALSE)
#' 
#' # Print results
#' print(fit)
#' fit
#' @keywords models
#' @export

print.abund <- function( x, 
                         criterion="AICc", 
                         maxBSFailPropForWarning = RdistanceControls()$maxBSFailPropForWarning,
                         ... ){
  #
  #   Print an object of class 'abund', which is class 'dfunc' with
  #   an abundance estimate stored in it.
  #

  print.dfunc( x, criterion=criterion )

  hasCI <- all(!is.null(x$density.ci))
  
  # --- Density printout ----
  if( hasCI ){
    mess <- c("Density in sampled area:", paste0(x$alpha*100, "% CI:"))
    mess <- format(mess, justify = "right")
    mess[2] <- substring(mess[2], 2) # remove pesky " " that happens with cat and \n
    ci <- paste( colorize(format(x$density.ci[1])), 
                 "to", 
                 colorize(format(x$density.ci[2])) )
    ptEst <- colorize( colorize(format(x$density)), col = "bold" )
    mess <- paste(mess, c(ptEst, ci))
  } else {
    mess <- c("Density in sampled area:")
    ptEst <- colorize( colorize(format(x$density)), col = "bold" )
    mess <- paste(mess, ptEst)
  }
  cat(paste0(mess, "\n"))

  # ---- Abundance printout ----
  cat("\n")  # blank line between for readability
  if( hasCI ){
    mess <- c(paste0( "Abundance in ", format(x$area), " study area:"), 
                      paste0(x$alpha*100, "% CI:"))
    mess <- format(mess, justify = "right")
    mess[2] <- substring(mess[2], 2) # remove pesky " " that happens with cat and \n
    ci <- paste( colorize(format(x$n.hat.ci[1])), 
                 "to", 
                 colorize(format(x$n.hat.ci[2])) )
    ptEst <- colorize( colorize(format(x$n.hat)), col = "bold" )
    mess <- paste(mess, c(ptEst, ci))
  } else {
    mess <- paste0( "Abundance in ", format(x$area), " study area:")
    ptEst <- colorize( colorize(format(x$n.hat)), col = "bold" )
    mess <- paste(mess, ptEst)
  }
  cat(paste0(mess, "\n"))
  
  if(!is.null(x$nItersConverged)){
    if(x$nItersConverged < nrow(x$B)) {
      cat(paste("CI based on", x$nItersConverged, "of", nrow(x$B), 
                "successful bootstrap iterations\n"))
      convRatio <- x$nItersConverged / nrow(x$B)
      if((1.0-convRatio) > maxBSFailPropForWarning) {
        warning("The proportion of non-convergent bootstrap iterations is high.", immediate. = TRUE)
        cat(paste0("The proportion of non-convergent bootstrap iterations exceeds ",
                  maxBSFailPropForWarning, ".\n",
                  "You should figure out why this happened (low detections, unstable dfunc form, etc.),\n",
                  "inspect the $B component of the abundance object (e.g., hist(x$B)), and decide whether the bootstrap CI is valid.\n"))
      }
    }
  }
  # cat( "\n" )

}
