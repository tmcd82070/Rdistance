#' @title Calculations for one bootstrap iteration
#' 
#' @description
#' Performs density and abundance estimation for one bootstrap iteration. 
#' 
#' 
#' @param pb A progress bar created with \code{progress::progress_bar$new()}.
#' 
#' @param plot.bs Logical. Whether to plot bootstrap estimate of detection function.
#' A plot must already exist because this uses \code{lines}.
#' 
#' @param plotCovValues Data frame containing values of covariates to plot.
#' Ignored if \code{plot.bs} is FALSE.
#' 
#' @inheritParams dE.single
#' 
#' @inheritParams abundEstim
#' 
#' @return A data frame containing density and abundance 
#' and other relevant statistics for 
#' one iteration of the bootstrap. 
#' 
#' @seealso \code{\link{bootstrap}}; \code{\link{abundEstim}}
#' 
#' @export
oneBsIter <- function(  object
                      , area 
                      , propUnitSurveyed
                      , pb
                      , plot.bs
                      , plotCovValues
                      , warn = FALSE
                      , asymptoticSE = FALSE
){
  
  # Stratified bootstrap: same rows from each strata, strata = is.na(effort)
  effCol <- attr(object$data, "effortColumn")
  missingTransect <- is.na(object$data[[effCol]]) 
  n <- length(missingTransect)
  noTranInd    <- sample( (1:n)[missingTransect], replace = T )
  withTranInd  <- sample( (1:n)[!missingTransect], replace = T )
  bsdf <- rbind( object$data[noTranInd, ]
               , object$data[withTranInd, ] ) # don't use dplyr::bind_rows
  
  # bsdf must be is.RdistDf(bsdf) = TRUE, it is
  
  # Fit dfunc to bs data
  dfunc.bs <- dfuncEstim(data = bsdf,
                         formula = object$formula,  
                         likelihood = object$likelihood, 
                         w.lo = object$w.lo,
                         w.hi = object$w.hi,
                         expansions = object$expansions, 
                         series = object$series,
                         x.scl = object$x.scl, 
                         g.x.scl = object$g.x.scl,
                         outputUnits = object$outputUnits,
                         warn = warn, 
                         asymptoticSE = asymptoticSE)

  # Note: Convergence is checked in estimateN. If nonConvergent, nEst$density returns NA
  nEst <- estimateN(
      dfunc.bs
    , area = area
    , propUnitSurveyed = propUnitSurveyed
  )

  if ( plot.bs ) {
    graphics::lines(dfunc.bs
          , newdata = plotCovValues
          , col = "blue"
          , lwd = 0.5
    )  
  }
  
  pb$tick()
  
  nEst        
}
