#' @name print.dfunc
#' 
#' @title Print a distance function object
#' 
#' @description Print method for distance functions produced by dfuncEstim, that are of class \code{dfunc}.
#' 
#' @param x An estimated distance function resulting from a call to \code{dfuncEstim}.
#' 
#' @param \dots Included for compatability with other print methods.  Ignored here.
#' 
#' @details The call, coefficients of the distanced function, whether the estimation converged, 
#' the likelihood and expansion function, and other statistics are printed.  At the bottom
#' of the output, the following quantities are printed,
#' \itemize{  
#'   \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
#'   \item \samp{Effective strip width or detection radius} : ESW or EDR as computed by \code{effectiveDistance}.
#'   \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
#'   Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
#'   \item \samp{Log likelihood} : Value of the maximized log likelihood.
#'   \item \samp{AIC} : Value of AIC for the distance function.
#' }
#' The number of digits printed is controled by \code{options()$digits}.
#' @return The input value of \code{obj} is invisibly returned.
#' @author Trent McDonald, WEST Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}}, \code{\link{print.abund}}
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData,
#'                     likelihood="halfnorm", w.hi=100, pointSurvey=FALSE)
#' 
#' # Print results
#' print(dfunc)
#' dfunc
#' @keywords models
#' @export
#' @importFrom stats pnorm

print.dfunc <- function( x, ... ){
#
#   Print a distance function
#

    cat("Call: ", deparse(x$call), "\n\n", sep = "")
    if (length(coef.dfunc(x))) {
        seCoef <- sqrt(diag(x$varcovar))
        waldZ <- coef.dfunc(x) / seCoef
        pWaldZ <- 2*pnorm(-abs(waldZ), 0, 1 )
        coefMat <- cbind(format(coef.dfunc(x)), format(seCoef), format(waldZ), format(pWaldZ))
        dimnames(coefMat)[[2]] <- c("Estimate", "SE", "z", "p(>|z|)")
        cat("Coefficients:\n")
        print.default(coefMat, print.gap = 2,
            quote = FALSE)
    } else {
      cat("No coefficients\n")
    }

    cat("\n")

    if( x$convergence == 0 ) {
      if(any(is.na(diag(x$varcov)))) {
        mess <- "FAILURE (singular variance-covariance matrix)"
      } else {
        mess <- "Success"
      }
    } else {
        mess <- paste( "FAILURE (Exit code=", x$convergence, ", ", x$fit$message, ")")
    }
    cat(paste("Convergence: ", mess,  "\n", sep=""))


    if( x$expansions==0 ){
        mess <- ""
    } else {
        mess <- paste( "with", x$expansions, "expansion(s) of", casefold( x$series, upper=TRUE ), "series")
    }
    cat(paste("Function:", casefold(x$like.form, upper=TRUE), mess, "\n") )

    cat(paste("Strip:", x$w.lo, "to", x$w.hi, "\n"))
    
    if( is.null(x$covars) ){
      if(x$pointSurvey){
        cat(paste("Effective detection radius (EDR):", format(effectiveDistance(x)), "\n"))
      } else {
        cat(paste("Effective strip width (ESW):", format(effectiveDistance(x)), "\n"))
      }
    } else {
      if(x$pointSurvey){
        cat(paste("Mean effective detection radius (EDR):", format(mean(effectiveDistance(x))), "\n"))
      } else {
        cat(paste("Mean effective strip width (ESW):", format(mean(effectiveDistance(x))), "\n"))
      }
    }
    
    cat(paste("Scaling: g(", x$x.scl, ") = ", format(x$g.x.scl), "\n", sep=""))
    cat(paste("Log likelihood:", format(x$loglik), "\n"))
    cat(paste("AIC:", format(AIC.dfunc(x)), "\n"))


    cat("\n")
    invisible(x)
}
