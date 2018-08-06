#' @name print.dfunc
#' 
#' @title Print a distance function object
#' 
#' @description Print method for distance functions produced by \code{dfuncEstim},
#' which are of class \code{dfunc}.
#' 
#' @param x An estimated distance function resulting from a call to \code{dfuncEstim}.
#' 
#' @param \dots Included for compatibility with other print methods.  Ignored here.
#' 
#' @param criterion A string specifying the criterion to print.
#' Must be one of "AICc" (the default), 
#' "AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas. 
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
#'   \item \samp{Criterion} : Value of the specified fit criterion (AIC, AICc, or BIC).
#' }
#' The number of digits printed is controlled by \code{options()$digits}.
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
#' dfunc
#' print(dfunc, criterion="BIC")
#' 
#' @keywords models
#' @export
#' @importFrom stats pnorm

print.dfunc <- function( x, criterion="AICc", ... ){
#
#   Print a distance function
#

    is.smoothed <- class(x$fit) == "density"
    
    callLine <- deparse(x$call)
    callLine <- paste(callLine, collapse = " ")
    callLine <- strwrap(paste0("Call: ",callLine),exdent=2)

    cat(paste0(callLine,"\n"))
    if (length(coef.dfunc(x))) {
        seCoef <- sqrt(diag(x$varcovar))
        waldZ <- coef.dfunc(x) / seCoef
        pWaldZ <- 2*pnorm(-abs(waldZ), 0, 1 )
        coefMat <- cbind(format(coef.dfunc(x)), format(seCoef), format(waldZ), format(pWaldZ))
        dimnames(coefMat)[[2]] <- c("Estimate", "SE", "z", "p(>|z|)")
        cat("Coefficients:\n")
        print.default(coefMat, print.gap = 2,
            quote = FALSE)
    } else if( is.smoothed ){
        cat(paste(x$fit$call[["kernel"]], "kernel smooth\n"))
        cat(paste(" Bandwidth method:", x$fit$call[["bw"]], "with adjustment factor", 
                    format(x$fit$call[["adjust"]]),"\n"))
        cat(paste(" Actual bandwidth =", format(x$fit$bw), "\n"))
    } else {
      cat("No coefficients\n")
    }

    cat("\n")

    if( !is.smoothed ){
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
    } 
    
    cat(paste("Strip:", x$w.lo, "to", x$w.hi, "\n"))
    
    effDist <- effectiveDistance(x)
    pDetect <- effDist / (x$w.hi - x$w.lo)
    if( is.null(x$covars) ){
      if(x$pointSurvey){
        cat(paste("Effective detection radius (EDR):", format(effDist), "\n"))
        cat(paste("Probability of detection:", format(pDetect^2), "\n"))
      } else {
        cat(paste("Effective strip width (ESW):", format(effDist), "\n"))
        cat(paste("Probability of detection:", format(pDetect), "\n"))
      }
    } else {
      if(x$pointSurvey){
        cat(paste("Average effective detection radius (EDR):", format(mean(effDist)), "\n"))
        cat(paste("Average probability of detection:", format(mean(pDetect^2)), "\n"))
      } else {
        cat(paste("Average effective strip width (ESW):", format(mean(effDist)), "\n"))
        cat(paste("Average probability of detection:", format(mean(pDetect)), "\n"))
      }
    }
    
    cat(paste("Scaling: g(", x$x.scl, ") = ", format(x$g.x.scl), "\n", sep=""))
    cat(paste("Log likelihood:", format(x$loglik), "\n"))
    if( !is.smoothed ){
      aic <- AIC.dfunc(x,criterion=criterion) 
      cat(paste0(attr(aic,"criterion"),": ", format(aic), "\n"))
    }


    cat("\n")
    invisible(x)
}
