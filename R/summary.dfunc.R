#' @title Summarize a distance function object
#' 
#' @description A summary method for distance functions 
#' produced by \code{dfuncEstim}, which are of class \code{dfunc}.
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
#' and the likelihood form are printed.  The following quantities are then printed,
#' \itemize{  
#'   \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
#'   \item \samp{Effective strip width or detection radius} : ESW or EDR as computed by \code{effectiveDistance}.
#'   \item \samp{Probability of Detection} : Probability of detecting a single target in the strip. 
#'   \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
#'   Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
#'   \item \samp{Log likelihood} : Value of the maximized log likelihood.
#'   \item \samp{Criterion} : Value of the specified fit criterion (AIC, AICc, or BIC).
#' }
#' The number of digits printed is controlled by \code{options()$digits}.
#' 
#' @return The input distance function object (\code{x}) is invisibly returned 
#' with additional components:
#' \itemize{
#'   \item \code{convMessage}: The convergence message. If the distance function
#'   is smoothed, the convergence message is NULL.
#'   \item \code{effDistance}: The ESW or EDR.
#'   \item \code{pDetect}: Probability of detection in the strip.
#'   \item \code{AIC}: AICc, AIC, or BIC of the fit, which ever was requested.
#'   \item \code{coefficients}: If the distance function has coefficients, the 
#'   coefficient matrix with standard errors, Z values, and p values. If the 
#'   distance function is smoothed, it has no coefficients and this component 
#'   is NULL.
#' }
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}}, 
#' \code{\link{print.abund}}, \code{\link{print.abund}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData)
#' 
#' # Print results
#' summary(dfunc)
#' summary(dfunc, criterion="BIC")
#' 
#' @keywords models
#' @export
#' @importFrom stats pnorm

summary.dfunc <- function( x, criterion="AICc", ... ){

    is.smoothed <- inherits( x$fit, "density" )

    callLine <- deparse(x$call)
    callLine <- paste(callLine, collapse = " ")
    callLine <- strwrap(paste0("Call: ",callLine),exdent=2)

    cat(paste0(callLine,"\n"))
    coefMat <- NULL
    convMessOut <- NULL
    if ( length(coef.dfunc(x)) & !is.smoothed ) {
      if( x$convergence == 0 ) {
        vcDiag <- diag(x$varcovar)
        if( any(is.na(vcDiag)) | any(vcDiag < 0.0)) {
          convMessOut <- "FAILURE (singular variance-covariance matrix)"
          convMess <- colorize("FAILURE", bg = "bgYellow")
          convMess <- paste(convMess, "(singular variance-covariance matrix)")
          seCoef <- rep(NA, length(diag(x$varcovar)))
          waldZ <- rep(NA, length(diag(x$varcovar)))
        } else {
          convMessOut <- "Success"
          convMess <- colorize("Success")
          seCoef <- sqrt(diag(x$varcovar))
          waldZ <- coef.dfunc(x) / seCoef
        }
      } else {
        convMessOut <- paste( "FAILURE", "(Exit code=", x$convergence, ", ", x$fit$message, ")")
        convMess <- colorize("FAILURE", col="white", bg = "bgRed")
        convMess <- paste( convMess, "(Exit code=", x$convergence, ", ", x$fit$message, ")")
        seCoef <- rep(NA, length(diag(x$varcovar)))
        waldZ <- rep(NA, length(diag(x$varcovar)))
      }
      pWaldZ <- 2*pnorm(-abs(waldZ), 0, 1 )
      coefMat <- cbind(coef.dfunc(x), seCoef, waldZ, pWaldZ)
      dimnames(coefMat)[[2]] <- c("Estimate", "SE", "z", "p(>|z|)")
      cat("Coefficients:\n")
      print(format(as.data.frame(coefMat)), print.gap = 2, quote = FALSE)
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
      cat(paste("Convergence: ", convMess,  "\n", sep=""))

      if( x$expansions==0 ){
          mess <- ""
      } else {
          mess <- paste( "with", x$expansions, "expansion(s) of", casefold( x$series, upper=TRUE ), "series")
      }
      cat(paste("Function:", colorize(casefold(x$like.form, upper=TRUE)), mess, "\n") )
    } 
    
    cat(paste("Strip:", colorize(format(x$w.lo)), "to", 
              colorize(format(x$w.hi)), "\n"))
    
    effDist <- effectiveDistance(x)
    pDetect <- effDist / (x$w.hi - x$w.lo) 
    pDetect <- units::drop_units(pDetect)  # units of pDetect should always be [1]
    if( is.null(x$covars) ){
      if(x$pointSurvey){
        mess <- "Effective detection radius (EDR):"
        pDetect <- pDetect^2
      } else {
        mess <- "Effective strip width (ESW):"
      }
      if( pDetect > 1 ){
        cat(paste(mess, 
                  colorize(format(effDist), col = "red"), 
                  colorize("> (w.hi - w.lo)", col = "red"), "\n"))
        cat(paste("Probability of detection:", 
                  colorize(format(pDetect), col = "red"),
                  colorize("> 1", col = "red"), "\n"))
      } else {
        cat(paste(mess 
                  , colorize(format(effDist))
                  , "\n"))
        if( all(!is.null(x$effDistance.ci)) ){
          ciMess <- paste0(
                         paste(rep(" ", nchar(mess) - 7), collapse = "")
                       , x$alpha*100
                       , "% CI: "
                       , colorize(format(x$effDistance.ci[1]))
                       , " to " 
                       , colorize(format(x$effDistance.ci[2])) 
                       , "\n"
                       ) 
          cat(ciMess)
        }
        cat(paste("Probability of detection:", 
                  colorize(format(pDetect)),
                  "\n"))
      }
    } else {
      if(x$pointSurvey){
        mess <- "Average effective detection radius (EDR):"
        cat(paste(mess, 
                  colorize(format(mean(effDist))), "\n"))
        if( all(!is.null(x$effDistance.ci)) ){
          ciMess <- paste0(
            paste(rep(" ", nchar(mess) - 7), collapse = "")
            , x$alpha*100
            , "% CI: "
            , colorize(format(x$effDistance.ci[1]))
            , " to " 
            , colorize(format(x$effDistance.ci[2])) 
            , "\n"
          ) 
          cat(ciMess)
        }
        cat(paste("Average probability of detection:", 
                  colorize(format(mean(pDetect^2))), "\n"))
      } else {
        mess <- "Average effective strip width (ESW):"
        cat(paste(mess
                , colorize(format(mean(effDist))), "\n"))
        if( all(!is.null(x$effDistance.ci)) ){
          ciMess <- paste0(
            paste(rep(" ", nchar(mess) - 7), collapse = "")
            , x$alpha*100
            , "% CI: "
            , colorize(format(x$effDistance.ci[1]))
            , " to " 
            , colorize(format(x$effDistance.ci[2])) 
            , "\n"
          ) 
          cat(ciMess)
        }
        cat(paste("Average probability of detection:", 
                  colorize(format(mean(pDetect))), "\n"))
      }
    }
    
    cat(paste("Scaling: g(", 
              colorize(format(x$x.scl)), ") = ", 
              colorize(format(x$g.x.scl)), sep=""))
    if(any(pDetect > 1.0)){
      cat(colorize(" <- One or more P(detect)>1: Check scaling", col = "red"))
      cat("\n")
    } else {
      cat("\n")
    }
    
    cat(paste("Negative log likelihood:", 
              colorize(format(x$loglik)), "\n"))
    
    if( !is.smoothed ){
      aic <- AIC.dfunc(x,criterion=criterion) 
      cat(paste0(attr(aic,"criterion"),": ", 
                 colorize(format(aic)), "\n"))
    }

    x$convMessage <- convMessOut
    x$effDistance <- effDist
    x$pDetect <- pDetect
    x$AIC <- aic
    x$coefficients <- coefMat

    invisible(x)
}
