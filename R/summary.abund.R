#' @title Summarize abundance estimates
#' 
#' @description Summarize an object of class \code{c("abund","dfunc")} 
#' that is output by \code{abundEstim}.
#' 
#' @inheritParams print.abund
#'
#' @inheritParams summary.dfunc
#'    
#' @details
#' If the proportion of bootstrap iterations that failed is 
#' greater than \code{getOption("Rdistance_maxBSFailPropForWarning")}, 
#' a warning about the validity of CI's is issued and 
#' a diagnostic message printed.  Increasing this option to a number greater 
#' than 1 will kill the warning (e.g., \code{options(Rdistance_maxBSFailPropForWarning = 1.3)}), 
#' but ignoring a large number of non-convergent
#' bootstrap iterations may be a bad idea (i.e., validity of the CI is 
#' questionable). The default value for \code{Rdistance_maxBSFailPropForWarning}
#' is 0.2.
#' 
#' @param \dots Included for compatibility to other print methods.  
#' Ignored here.
#' 
#' @return 0 is invisibly returned.
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}, 
#' \code{\link{summary.dfunc}}, \code{\link{print.dfunc}}, 
#' \code{\link{print.abund}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDf)
#' 
#' # Fit half-normal detection function
#' dfunc <- sparrowDf |> dfuncEstim(formula=dist ~ 1 + offset(groupsize))
#' 
#' # Estimate abundance given the detection function
#' # Note: should do more than R=20 bootstrap iterations
#' fit <- abundEstim(dfunc
#'                 , area = units::set_units(4105, "km^2")
#'                 , R=20
#'                 , ci=0.95)
#' 
#' summary(fit)
#' 
#' @keywords models
#' @export

summary.abund <- function( object
                         , criterion="AICc"
                         , ... ){
  
  summary.dfunc( object = object, criterion=criterion )
  cat("\n")
  hasCI <- !is.null(object$B) && (nrow(object$B) > 0)
  ests <- object$estimates
  
  # ---- Groupsize printout ----
  gs <- Rdistance::groupSizes(object)
  mess <- format(c(
                  "Surveyed Units:"
                , "Individuals seen:"
                , "Average group size:"
                , "Group size range:"), justify = "right")
  mess[1] <- paste0(" ", mess[1]) # pesky " " that happens with cat and \n
  avgGs <- c(
             colorize(format(ests$surveyedUnits))
           , paste( colorize(format(ests$nSeen)), "in", colorize(format(ests$nGroups)), "groups")
           , colorize( format( ests$avgGroupSize ))
           , paste(colorize(format( min(gs, na.rm = TRUE) ))
                          , "to"
                          , colorize(format( max(gs, na.rm = TRUE) ))))
  mess <- paste(mess, avgGs)
  cat(paste(mess, "\n"))
  
  if(hasCI){
    cat("\n")
  }
  
  # ---- Density printout ----
  if( hasCI ){
    mess <- c("Density in sampled area:", paste0(object$ci*100, "% CI:"))
    mess <- format(mess, justify = "right")
    mess[2] <- substring(mess[2], 2) # remove pesky " " that happens with cat and \n
    ci <- paste( colorize(format(ests$density_lo)), 
                 "to", 
                 colorize(format(ests$density_hi)) )
    ptEst <- colorize( colorize(format(ests$density)), col = "bold" )
    mess <- paste(mess, c(ptEst, ci))
  } else {
    mess <- c("Density in sampled area:")
    ptEst <- colorize( colorize(format(ests$density)), col = "bold" )
    mess <- paste(mess, ptEst)
  }
  cat(paste0(mess, "\n"))

  # ---- Abundance printout ----
  if(hasCI){
    cat("\n")  # blank line between for readability
  }
  if( hasCI ){
    mess <- c(paste0( "Abundance in ", format(ests$area), " study area:"), 
                      paste0(object$ci*100, "% CI:"))
    mess <- format(mess, justify = "right")
    mess[2] <- substring(mess[2], 2) # remove pesky " " that happens with cat and \n
    ci <- paste( colorize(format(ests$abundance_lo)), 
                 "to", 
                 colorize(format(ests$abundance_hi)) )
    ptEst <- colorize( colorize(format(ests$abundance)), col = "bold" )
    mess <- paste(mess, c(ptEst, ci))
  } else {
    mess <- paste0( "Abundance in ", format(ests$area), " study area:")
    ptEst <- colorize( colorize(format(ests$abundance)), col = "bold" )
    mess <- paste(mess, ptEst)
  }
  cat(paste0(mess, "\n"))
  
  if( hasCI ){
    nItersConverged <- sum(!is.na(object$B$density))
    if(nItersConverged < nrow(object$B)) {
      cat(paste("CI based on", nItersConverged, "of", nrow(object$B), 
                "successful bootstrap iterations\n"))
      convRatio <- nItersConverged / nrow(object$B)
      if((1.0-convRatio) > getOption("Rdistance_maxBSFailPropForWarning")) {
        warning("The proportion of non-convergent bootstrap iterations is high.", immediate. = TRUE)
        cat(paste0("The proportion of non-convergent bootstrap iterations exceeds ",
                  getOption("Rdistance_maxBSFailPropForWarning"), ".\n",
                  "It would be good to figure out why this happened (low detections, unstable dfunc form, etc.),\n",
                  "inspect the $B component of the abundance object (e.g., hist(object$B$density)), and decide whether the bootstrap CI is valid.\n"))
      }
    }
  }

  invisible(0)
}
