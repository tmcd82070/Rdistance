#' @title Summarize a distance function object
#' 
#' @description A summary method for distance functions. 
#' Distance functions are produced by 
#' \code{dfuncEstim} (class \code{dfunc}).
#' 
#' @param x An estimated distance function resulting 
#' from a call to \code{dfuncEstim}.
#' 
#' @param \dots Included for compatibility with other print methods.  Ignored here.
#' 
#' @param criterion A string specifying the model fit criterion to print.
#' Must be one of "AICc" (the default), 
#' "AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas. 
#' 
#' @details This function first prints the following 
#' quantities:
#' \itemize{  
#'   \item \samp{Call} : The original function call.
#'   \item \samp{Coefficients} : A matrix of estimated coefficients, their 
#'   standard errors, and Wald Z tests.
#'   \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
#'   \item \samp{Effective strip width or detection radius} : ESW or EDR as computed by \code{effectiveDistance}.
#'   \item \samp{Probability of Detection} : Probability of detecting a single target in the strip. 
#'   \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
#'   Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
#'   \item \samp{Log likelihood} : Value of the maximized log likelihood.
#'   \item \samp{Criterion} : Value of the specified fit criterion (AIC, AICc, or BIC).
#' }
#' The number of digits used in the printout is 
#' controlled by \code{options()$digits}.
#' 
#' @return The input distance function object (\code{x}) is invisibly returned, 
#' with the following additional components:
#' \itemize{
#'   \item \code{convMessage}: The convergence message. If the distance function
#'   is smoothed, the convergence message is NULL.
#'   \item \code{effDistance}: The ESW or EDR.
#'   \item \code{pDetect}: Probability of detection in the strip.
#'   \item \code{AIC}: AICc, AIC, or BIC of the fit, which ever was requested.
#'   \item \code{coefficients}: If the distance function has coefficients, this 
#'   is the coefficient matrix with standard errors, Wald Z values, and p values. 
#'   If the distance function is smoothed, it has no coefficients and this component 
#'   is NULL.
#' }
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}}, 
#' \code{\link{print.abund}}, \code{\link{print.abund}}
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' 
#' # Fit half-normal detection function
#' dfunc <- sparrowDf |> dfuncEstim(formula=dist~1)
#' 
#' # Print results
#' summary(dfunc)
#' summary(dfunc, criterion="BIC")
#' 
#' @keywords models
#' @export
#' @importFrom stats pnorm

summary.dfunc <- function( x, criterion="AICc", ... ){

  x <- print.dfunc(x, ...)
  
  # Convergence and likelihood line ----
  if( !(isSmooth <- is.smoothed(x)) ){
    if( x$convergence == 0){
      # b/c FAILURE mess printed in 'print.dfunc', but not Success
      cat("\n")
      cat(paste("Convergence: ", x$convMessage,  "\n", sep=""))
    }

    if( x$expansions==0 ){
        mess <- ""
    } else {
        mess <- paste( "with", x$expansions, "expansion(s) of", casefold( x$series, upper=TRUE ), "series")
    }
    cat(paste("Function:", colorize(casefold(x$likelihood, upper=TRUE)), mess, "\n") )
  } 
    
  # Strip line ----
  cat(paste("Strip:", colorize(format(x$w.lo)), "to", 
            colorize(format(x$w.hi)), "\n"))
  
  # Effective distance line ----
  effDist <- effectiveDistance(x)
  pDetect <- effDist / (x$w.hi - x$w.lo) 
  pDetect <- units::set_units(pDetect, NULL)  # units of pDetect should always be [1]
  interceptOnly <- intercept.only(x)

  if( is.points(x) ){
    # Points
    pDetect <- pDetect^2 # needed later, for P(detect) line
    if( interceptOnly ){
      mess <- "Effective detection radius (EDR):"
    } else {
      mess <- "Average effective detection radius (EDR):"
    }
  } else {
    # Lines
    if( interceptOnly ){
      mess <- "Effective strip width (ESW):"
    } else {
      mess <- "Average effective strip width (ESW):"
    }
  }
  
  if(interceptOnly){
    efd <- effDist[1]
  } else {
    efd <- mean(effDist)
  }
  mess <- paste(mess, colorize(format(efd)))
  cat(paste(mess)) # no return here, wait for ciMess to print at end of line
  
  # EFD CI line ----
  if( interceptOnly && all(!is.null(x$effDistance.ci)) ){
    ciMess <- paste0(" "
      , x$alpha*100
      , "% CI: "
      , colorize(format(x$effDistance.ci[1]))
      , " to " 
      , colorize(format(x$effDistance.ci[2])) 
    ) 
  } else if( !interceptOnly ){
    ciMess <- paste0(
                  " (range "
                , colorize(format(min(effDist)))
                , " to "
                , colorize(format(max(effDist)))
                , ")"
                )
  } else {
    ciMess <- ""
  }
  cat(paste(ciMess,"\n"))
  
  # pDetect line ----
  if(interceptOnly){
    if( pDetect[1] > 1 ){
      cat(paste("Probability of detection:"
                , colorize(format(pDetect[1]), col = "red")
                , colorize("> 1", col = "red")))
    } else {
      cat(paste("Probability of detection:" 
                , colorize(format(pDetect[1]))
                ))
    }
  } else {
    if( any(pDetect > 1) ){
      ng1 <- sum(pDetect > 1)
      np <- length(pDetect)
      cat(paste("Average probability of detection:", 
                colorize(format(mean(pDetect))),
                colorize(paste(ng1, "of", np, "P(detect) > 1")
                         , col = "red")))
      
    } else {
      cat(paste0("Average probability of detection: " 
                , colorize(format(mean(pDetect)))
                , " (range "
                , colorize(format(min(pDetect)))
                , " to "
                , colorize(format(max(pDetect)))
                , ")"
                ))
    }
  }
  cat("\n")
    
  # Scaling line ----    
  cat(paste("Scaling: g(", 
            colorize(format(x$x.scl)), ") = ", 
            colorize(format(x$g.x.scl)), sep=""))
  if(any(pDetect > 1.0)){
    cat(colorize(" <- Check scaling", col = "red"))
  } 
  cat("\n")
    
  # Log likelihood line ----    
  cat(paste("Log likelihood:", 
            colorize(format(x$loglik)), "\n"))
  
  # AIC line ----
  if( !isSmooth ){
    aic <- AIC.dfunc(x,criterion=criterion) 
    cat(paste0(attr(aic,"criterion"),": ", 
               colorize(format(aic)), "\n"))
  }

  # Final assignments ----
  x$effDistance <- effDist
  x$pDetect <- pDetect
  x$AIC <- aic

  invisible(x)
}
