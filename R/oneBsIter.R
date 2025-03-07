#' @title oneBsIter - Computations for one bootstrap iteration
#' 
#' @description
#' An internal (un-exported) function to perform density and 
#' abundance calculations on one iteration of the bootstrap. 
#' 
#' @param indexDf A data frame containing row indices to use 
#' for subsetting the rows of \code{data}. The actual indicies are 
#' in column \code{rowIndex}.
#' 
#' @param key A data frame containing the current id of the 
#' BS iteration.  This is included for compatability with 
#' \code{dplyr::group_modify}, but it is not used internally.
#' The original non-resampled data have \code{key == "Original"}.
#' 
#' @param data An Rdistance nested data frame containing 
#' the data to bootstrap resample.  Rows of this data frame, 
#' equating to transects, are sampled using the indicies in 
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
#' and other relavant statistics for 
#' one iteration of the bootstrap. 
#' 
#' @importFrom graphics lines
#' 
oneBsIter <- function(indexDf
                      , key
                      , data
                      , formula
                      , likelihood
                      , w.lo
                      , w.hi
                      , expansions
                      , series
                      , x.scl
                      , g.x.scl
                      , outputUnits
                      , warn
                      , area 
                      , propUnitSurveyed
                      , pb
                      , plot.bs
                      , plotCovValues
){
  
  bsdf <- data[indexDf$rowIndex,]
  
  dfunc.bs <- Rdistance::dfuncEstim(data = bsdf,
                         formula = formula,  
                         likelihood = likelihood, 
                         w.lo = w.lo,
                         w.hi = w.hi,
                         expansions = expansions, 
                         series = series,
                         x.scl = x.scl, 
                         g.x.scl = g.x.scl,
                         outputUnits = outputUnits,
                         warn = warn)

  # Note: Convergence is checked in estimateN. If nonConvergent, nEst$density returns NA
  nEst <- Rdistance::estimateN(
      dfunc.bs
    , area = area
    , propUnitSurveyed = propUnitSurveyed
  )

  Coefs <- data.frame(matrix(coef(dfunc.bs), nrow = 1))
  names(Coefs) <- names(coef(dfunc.bs))
  
  if(Rdistance::is.points(data)){
    avgEDD <- mean( sqrt(nEst$pDetection) * nEst$w, na.rm = TRUE)
  } else {
    avgEDD <- mean( nEst$pDetection * nEst$w, na.rm = TRUE)
  }
  
  out <- tibble::tibble(
    Coefs
    , density = nEst$density
    , abundance = nEst$abundance
    , nGroups = nEst$n.groups
    , nSeen = nEst$n.seen
    , area = nEst$area
    , surveyedUnits = nEst$surveyedUnits
    , avgGroupSize = nEst$avg.group.size
    , avgEffDistance = avgEDD
  )
  
  if ( plot.bs ) {
    graphics::lines(dfunc.bs
          , newdata = plotCovValues
          , col = "blue"
          , lwd = 0.5
    )  
  }
  
  pb$tick()
  
  out        
}