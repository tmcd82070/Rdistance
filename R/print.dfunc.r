#' @name print.dfunc
#' @aliases print.dfunc
#' @title Print a distance function object.
#' @description Print method for distance functions produced by F.dfunc.estim, that are of class \code{dfund}.
#' @usage \method{print}{dfunc}(x, ...)
#' @param x An estimated distance function resulting from a call to \code{F.dfunc.estim}.
#' @param \dots Included for compatability with other print methods.  Ignored here.
#' @details The call, coefficients of the distanced function, whether the estimation converged, 
#' the likelihood and expansion function, and other statistics are printed.  At the bottom
#' of the output, the following quantities are printed,
#' \itemize{  
#'   \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
#'   \item \samp{Effective strip width} : Effective strip half-width as computed by \code{ESW}.
#'   \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
#'   Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
#'   \item \samp{Log likelihood} : Value of the maximized log likelihood.
#'   \item \samp{AIC} : Value of AIC for the distance function.
#' }
#' The number of digits printed is controled by \code{options()$digits}.
#' @return The input value of \code{obj} is invisibly returned.
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}}, \code{\link{plot.dfunc}}, \code{\link{print.abund}}
#' @examples # Load the example datasets of sparrow detections and transects from package
#' data(sparrow.detections)
#' data(sparrow.transects)
#' 
#' # Fit detection function to perpendicular, off-transect distances
#' dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)
#' 
#' # Print the output                 
#' print(dfunc)
#' @keywords models

print.dfunc <- function( x, ... ){
#
#   Print a distance function
#

    cat("Call: ", deparse(x$call), "\n\n", sep = "")
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x)), print.gap = 2,
            quote = FALSE)
    }
    else cat("No coefficients\n")

    cat("\n")

    if( x$convergence == 0 ){
        mess <- "Success"
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
    if(x$point.transects){
      cat(paste("Effective radius:", format(effective.radius(x)), "\n"))
    }
    else{
      cat(paste("Effective strip width:", format(ESW(x)), "\n"))
    }
    
    cat(paste("Scaling: g(", x$x.scl, ") = ", format(x$g.x.scl), "\n", sep=""))
    cat(paste("Log likelihood:", format(x$loglik), "\n"))
    cat(paste("AIC:", format(AIC(x)), "\n"))


    cat("\n")
    invisible(x)
}
