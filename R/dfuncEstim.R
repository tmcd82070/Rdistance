#' @title dfuncEstim - Estimate a distance-based detection function
#' 
#' @description Fits a detection function using maximum likelihood. 
#'
#' @inheritDotParams dE.lt.single formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' @inheritDotParams dE.lt.multi  formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' @inheritDotParams dE.pt.single formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' @inheritDotParams dE.pt.multi  formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' 
#' @param data An \code{RdistDf} data frame. \code{RdistDf} data frames 
#' contain one line per transect and a list-based column. The list-based
#' column contains a data frame with detection information. 
#' The detection information data frame on each row contains (at least) distances 
#' and group sizes of all targets detected on the transect.
#' Function \code{\link{RdistDf}} creates \code{RdistDf} data frames 
#' from separate transect and detection data frames. 
#' \code{\link{is.RdistDf}} checks whether data frames
#' are \code{RdistDf}'s. 
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
  if( !Rdistance::is.RdistDf(data) ){
    stop(paste(deparse(substitute(data)), "is not an RdistDf. See help('RdistDf')"))
  }
  
  # checkUnits is part of checkRdistDf
  # get data checks from 'master' branch and put them in checkRdistDf.
  checkRdistDf(data)

  if( attr(data, "transType") == "point" ){
    # Point transects ----
    res <- switch( attr(data, "obsType"),
              "single" = dE.pt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.pt.multi(data, ...)
            , stop(dfuncEstimErrMessage("observer system", "obsType"))
            )
  } else if(attr(data, "transType") == "line"){
      # Line transects ----
      res <- switch( attr(data, "obsType")
            , "single" = dE.lt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.lt.multi(data, ...)
            , stop(dfuncEstimErrMessage("observer system", "obsType"))
      )
  } else {
    stop(dfuncEstimErrMessage("transect type", "transType"))
  }
  
  res 
}

