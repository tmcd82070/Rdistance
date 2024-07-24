#' @title print.dfunc - Print method for distance function object
#' 
#' @description Print method for distance function objects produced 
#' by \code{dfuncEstim}.
#' 
#' @param x An estimated distance function, usually the result 
#' of calling to \code{dfuncEstim}.
#' 
#' @param \dots Included for compatibility with other print methods.  Ignored here.
#' 
#' @return The input distance function (\code{x}) is returned invisibly.
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}}, 
#' \code{\link{print.abund}}, \code{\link{summary.dfunc}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' dfunc <- sparrowDf |> dfuncEstim(formula=dist~1)
#' 
#' dfunc
#' 
#' @export
#' @importFrom stats pnorm
print.dfunc <- function( x, ... ){

    isSmooth <- is.smoothed(x)
    fuzz <- getOption("Rdistance_fuzz")

    callLine <- deparse(x$call)
    callLine <- paste(callLine, collapse = " ")
    callLine <- strwrap(paste0("Call: ",callLine),exdent=2)

    cat(paste0(callLine,"\n"))
    
    coefs <- coef(x)
    if ( length(coefs) & !isSmooth ) {
      vcDiag <- diag(x$varcovar)
      seCoef <- vcDiag
      seCoef[seCoef < fuzz] <- NA # fuzz is positive, so this NAs all negatives
      seCoef <- sqrt(seCoef)      # Avoids warning re sqrt of negative
      waldZ <- coefs / seCoef
      if( x$convergence == 0 ) {
        if( any(is.na(seCoef)) ) {
          mess <- colorize("VARIANCE FAILURE", col = "black", bg = "bgYellow")
          mess <- paste(mess, "(singular variance-covariance matrix)")
        } else {
          mess <- colorize("Success")
        }
      } else {
        mess <- colorize("FAILURE", col="white", bg = "bgRed")
        mess <- paste0( mess, " (Exit code= ", x$convergence, ", ", x$message, ")")
      }
      pWaldZ <- 2*pnorm(-abs(waldZ), 0, 1 )
      coefMat <- cbind(format(coefs)
                       , format(seCoef)
                       , format(waldZ)
                       , format(pWaldZ))
      dimnames(coefMat)[[2]] <- c("Estimate", "SE", "z", "p(>|z|)")
      cat("Coefficients:\n")
      print.default(coefMat, print.gap = 2, quote = FALSE)
      if( !grepl("Success", mess) ){
        cat("\n")
        cat(paste("Convergence: ", mess,  "\n", sep=""))
      } 
    } else if( isSmooth ){
      cat(paste(x$fit$call[["kernel"]], "kernel smooth\n"))
      cat(paste(" Bandwidth method:", x$fit$call[["bw"]], "with adjustment factor", 
                  format(x$fit$call[["adjust"]]),"\n"))
      cat(paste(" Actual bandwidth =", format(x$fit$bw), "\n"))
    } else {
      cat("No coefficients\n")
      coefMat <- NULL
      mess <- "Rdistance data"
    }

    x$coefficients <- coefMat
    x$convMessage <- mess
    invisible(x)
}
