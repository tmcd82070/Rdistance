#' @title print.dfunc - Print method for distance function object
#' 
#' @description Print method for distance function objects produced 
#' by \code{dfuncEstim}.
#' 
#' @param x An estimated detection function object, normally 
#' produced by calling \code{\link{dfuncEstim}}.
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
#' data(sparrowSiteData)
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

  # This routine must handle x with estimated coefficients and 
  # x that is just an mf object. 
  
  if( is.null(x$call) ){
    # this is what happens when x is just a model frame
    callLine <- paste("Rdistance Model:"
                  , colorize("No coefficients"))
    cat(paste0(callLine,"\n"))
    callLine <- paste("  "
                    , "Formula:"
                    , colorize(format(x$formula)))
    cat(paste0(callLine,"\n"))
    summary(  object = x$data
            , formula = x$formula
            , w.lo = x$w.lo
            , w.hi = x$w.hi)
    return(invisible(x))
  } 
  
  # If we are here, x has coefficients and is a regular 
  # estimated model.
  callLine <- deparse(x$call)
  callLine <- paste(callLine, collapse = " ")
  callLine <- strwrap(paste0("Call: ",callLine),exdent=2)
  cat(paste0(callLine,"\n"))

  isSmooth <- is.smoothed(x)
  fuzz <- getOption("Rdistance_fuzz")
  
  coefs <- coef(x)
  if( is.null(x$varcovar) && !isSmooth ){
    
    # No bootstraps yet, SE's pending
    coefMat <- cbind(format(coefs))
    dimnames(coefMat)[[2]] <- c("Estimate")
    if( x$convergence == 0 ) {
      mess <- paste0(colorize("Success")
                   , "; "
                   , colorize("SE's pending bootstrap"))
    } else {
      mess <- colorize("FAILURE", col="white", bg = "bgRed")
      mess <- paste0( mess, " (Exit code= ", x$convergence, ", ", x$message, ")")
    }

  } else if ( !isSmooth ) {
    # X has a varcovar, either asymptotic or bootstrap
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
        # IF YOU CHANGE EITHER OF THESE MESSAGES, MAKE SURE TO 
        # CHANGE grepl IN IF STATEMENT MARKED WITH **** BELOW! 
        # ALSO! YOU NEED TO CHANGE THE SAME grepl STATEMENT 
        # IN SUMMARY.DFUNC (MARKED BY ****)
        if( is.null(x$B) ){
          mess <- paste0(mess, "; ", colorize("Asymptotic SE's"))
        } else {
          mess <- paste0(mess, "; ", colorize("Bootstrap SE's"))
        }
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
  }
  cat("Coefficients:\n")
  print.default(coefMat, print.gap = 2, quote = FALSE)
  
  # **** CHANGE THIS grepl IF SUCCESSFUL MESSAGE CHANGES
  if( !grepl("(Asymptotic|Bootstrap) SE's", mess)  ){ 
    cat("\n")
    cat(paste("Message: ", mess,  "\n", sep=""))
  } 
  
  if( isSmooth ){
    cat(paste(x$fit$call[["kernel"]], "kernel smooth\n"))
    cat(paste(" Bandwidth method:", x$fit$call[["bw"]], "with adjustment factor", 
                format(x$fit$call[["adjust"]]),"\n"))
    cat(paste(" Actual bandwidth =", format(x$fit$bw), "\n"))
  } else if(length(coefs) == 0){
    cat("No coefficients\n")
    coefMat <- NULL
    mess <- "Rdistance data"
  }

  x$coefficients <- coefMat
  x$convMessage <- mess
  invisible(x)
}
