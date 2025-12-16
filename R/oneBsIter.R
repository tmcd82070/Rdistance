#' @title Computations for one bootstrap iteration
#' 
#' @description
#' An internal (un-exported) function to perform density and 
#' abundance calculations on one iteration of the bootstrap. 
#' 
#' @param indexDf A data frame containing row indices to use 
#' for subsetting the rows of \code{data}. The actual indices are 
#' in column \code{rowIndex}.
#' 
#' @param key A data frame containing the current id of the 
#' BS iteration.  This is included for compatibility with 
#' \code{dplyr::group_modify}, but it is not used internally.
#' The original non-resampled data have \code{key == "Original"}.
#' 
#' @param data An Rdistance nested data frame containing 
#' the data to bootstrap resample.  Rows of this data frame, 
#' equating to transects, are sampled using the indices in 
#' \code{indexDf$rowIndex}. 
#' 
#' @param pb A progress bar created with \code{progress::progress_bar$new()}.
#' 
#' @param plot.bs Logical. Whether to plot bootstrap estimate of detection function.
#' A plot must already exist because this uses \code{lines}.
#' 
#' @param plotCovValues Data frame containing values of covariates to plot, 
#' if \code{plot.bs} is TRUE.
#' 
#' @inheritParams dE.single
#' 
#' @inheritParams abundEstim
#' 
#' @return A data frame containing density and abundance 
#' and other relevant statistics for 
#' one iteration of the bootstrap. 
#' 
#' 
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
