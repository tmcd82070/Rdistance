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
#' # Sparrow line transect example
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' 
#' dfunc <- dfuncEstim(sparrowDf, 
#'                     formula = dist ~ 1
#'                   )
#'                   
#' dfunc <- sparrowDf |> 
#'          dfuncEstim( 
#'            formula = dist ~ observer
#'          )
#'              
#' dfunc
#' summary(dfunc)
#' plot(dfunc)                   
#'
#' @keywords model
#' @export

dfuncEstim <- function (  data, ... ){

  call <- match.call()
  tranType <- attr(data, "transType")
  obsType <- attr(data, "obsType")

  # Dispatch separate estimation functions based on transect and observer types ----
  if( tranType == "point" ){
    # Point transects ----
    res <- switch( obsType
            , "single" = dE.pt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.pt.multi(data, ...)
            , stop(dfuncEstimErrMessage("observer system", "obsType"))
            )
  } else if( tranType == "line"){
      # Line transects ----
      res <- switch( obsType
            , "single" = dE.lt.single(data, ...)
            , "1|2"    =
            , "2|1"    =
            , "both"   = dE.lt.multi(data, ...)
            , stop(dfuncEstimErrMessage("observer system", "obsType"))
      )
  } else {
    stop(dfuncEstimErrMessage("transect type", "transType"))
  }

  # Assign common elements, and class ==== 
  res$call <- call
  
  # nCovars ----
  # Can always count number of covariates later, in methods, but store 
  # here to save some time and code.
  # nCovars meanings:
  #   nCovars < 0 : model has 'nCovars' covariates but no intercept, 
  #                 e.g., dist ~ -1 + observer + height => nCovars = -2
  #   nCovars == 0 : model has (Intercept) only
  #   nCovars > 0 : model has 'nCovars' besides intercept,
  #                 e.g., dist ~ observer + height => nCovars = 2

  nCovars <- length(attr(terms(res$mf), "term.labels"))
  if( attr(terms(res$mf), "intercept") == 0 ){
    nCovars <- -nCovars  
  }
  res$nCovars <- nCovars
  
  # Estimated function type ----
  if( !( res$likelihood %in% c("Gamma","smu")) ){
    res$LhoodType <- "parametric" # halfnorm, hazrate, etc., all those
  } else {
    res$LhoodType <- res$likelihood  # Gamma and smu
  }

  res 
}

