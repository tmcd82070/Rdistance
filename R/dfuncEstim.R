#' @title dfuncEstim - Estimate a distance-based detection function
#' 
#' @description Fits a detection function using maximum likelihood. 
#'
#' @inheritDotParams dE.lt.single formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' 
#' @inheritParams dE.lt.single
#' 
#' @inherit dE.lt.single details
#' 
#' @inheritSection dE.lt.single Group Sizes
#' 
#' @inheritSection dE.lt.single Contrasts
#' 
#' @inheritSection dE.lt.single Measurement Units
#' 
#' @inherit dE.lt.single return
#'     
#' @references Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'     
#' @seealso \code{\link{abundEstim}}, \code{\link{autoDistSamp}}.
#' Likelihood-specific help files (e.g., \code{\link{halfnorm.like}}). 
#' See package vignettes for additional options. 
#' 
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' dfunc <- dfuncEstim(formula = dist ~ 1
#'                   , detectionData = sparrowDetectionData)
#' dfunc
#' plot(dfunc)                   
#'
#' @keywords model
#' @export
#' @importFrom stats nlminb model.response is.empty.model 
#' @importFrom stats model.matrix contrasts optim
#' @import units

dfuncEstim <- function (  data, ... ){

  # Check validity of data ----
  # checkUnits is part of checkRdistDf
  # checkRdistDf(data)

  errUnk <- function(txt, attri){
    paste0( "Unknown "
             , txt
             , ". Set "
             , txt
             , " using 'RdistDf',"
             , " or with "
             , 'attr(data, "'
             , attri
             , '") <- "value".'
             , " See help(RdistDf) for list of values."
    )
  } 
  
  if( attr(data, "transType") == "point" ){
    # Point transects ----
    res <- switch( attr(data, "obsType"),
              "single" = dE.pt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.pt.multi(data, ...)
            , stop(errUnk("observer system", "obsType"))
            )
  } else if(attr(data, "transType") == "line"){
      # Line transects ----
      res <- switch( attr(data, "obsType")
            , "single" = dE.lt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.lt.multi(data, ...)
            , stop(errUnk("observer system", "obsType"))
      )
  } else {
    stop(errUnk("transect type", "transType"))
  }
  
  res 
}

