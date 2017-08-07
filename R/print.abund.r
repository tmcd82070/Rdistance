#' @name print.abund
#' @aliases print.abund
#' @title Print abundance estimates.
#' @description Print an object of class c("abund","dfunc") that is output by \code{F.abund.estim}.
#' @usage \method{print}{abund}(x, ...)
#' @param x An object output by \code{F.abund.estim}.  This is a distance function object that 
#'   also contains abundance estimates, and has class c("abund", "dfunc").
#' @param \dots Included for compatibility to other print methods.  Ignored here.
#' @details The default print method for class 'dfunc' is called, then the abundance estimates 
#'   contained in \code{obj} are printed.
#' @return No value is returned.
#' @author Trent McDonald, WEST Inc., \email{tmcdonald@west-inc.com}
#' @seealso \code{\link{F.dfunc.estim}}, \code{\link{F.abund.estim}}
#' @examples # Load the example datasets of sparrow detections and transects from package
#'   data(sparrow.detections)
#'   data(sparrow.transects)
#'   
#'   # Fit detection function to perpendicular, off-transect distances
#'   dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)
#'   
#'   # Estimate abundance given a detection function
#'   fit <- F.abund.estim(dfunc, detection.data=sparrow.detections, transect.data=sparrow.transects,
#'                        area=10000, R=10, ci=0.95, plot.bs=TRUE, by.id=FALSE)
#'   
#'   # Print the output                 
#'   print(fit)
#' @keywords models
#' @export

print.abund <- function( x, ... ){
#
#   Print an object of class 'abund', which is class 'dfunc' with
#   an abundance estimate stored in it.
#

print.dfunc( x )

cat( paste( "Abundance estimate: ", format(x$n.hat), "; ",
        paste(x$alpha*100, "% CI=(", sep=""), format(x$ci[1]), 
        "to", format(x$ci[2]),
        ")\n"))
if(any(is.na(x$B))) cat(paste("CI based on", sum(!is.na(x$B)), "of", length(x$B), "successful bootstrap iterations\n"))        
cat( "\n" )

}
