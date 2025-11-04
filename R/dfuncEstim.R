#' @title Estimate a distance-based detection function
#' 
#' @description Fits a detection function using maximum likelihood. 
#'
#' @inheritDotParams dE.single formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
#' @inheritDotParams dE.multi  formula likelihood w.lo w.hi expansions series x.scl g.x.scl warn outputUnits
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
#' @inherit dE.single details
#' 
#' @inheritSection dE.single Group Sizes
#' 
#' @inheritSection dE.single Contrasts
#' 
#' @inheritSection dE.single Measurement Units
#' 
#' @inherit dE.single return
#'     
#' @references Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'     
#' @seealso \code{\link{abundEstim}}, \code{\link{autoDistSamp}}.
#' Likelihood-specific help files (e.g., \code{\link{halfnorm.like}}). 
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
#' summary(dfunc)
#' 
#'   
#' data(sparrowDfuncObserver) # pre-estimated object
#' \dontrun{                 
#' # Commands that produced 'sparrowDfuncObserver'
#' sparrowDfuncObserver <- sparrowDf |> 
#'          dfuncEstim( 
#'            formula = dist ~ observer
#'          )
#' }     
#' sparrowDfuncObserver
#' summary(sparrowDfuncObserver)
#' plot(sparrowDfuncObserver)
#' plot(sparrowDfuncObserver
#'    , newdata = data.frame(observer = c("obs1", "obs2", "obs3"
#'                                      , "obs4", "obs5")))
#'
#' @export

dfuncEstim <- function (  data, ... ){

  call <- match.call()
  obsType <- Rdistance::observationType(data)
  
  # because Rdistance can override some options, e.g. optimizer
  # for oneStep, save a copy of options so can restore later.
  op <- options()
  op <- op[grepl("Rdistance_", names(op))]

  # Dispatch separate estimation functions based on observer type ----
  res <- switch( obsType
          , "single" = dE.single(data, ...)
          , "1|2"    =
          , "2|1"    =
          , "both"   = dE.multi(data, ...)
          , stop(dfuncEstimErrMessage("observer system", "obsType"))
          )

  # Assign common elements, and class ==== 
  res$call <- call
  res$dataName <- deparse(substitute(data))
  
  # nCovars ----
  # Can always count number of covariates later, in methods, but store 
  # here to save some time and code.
  # nCovars meanings:
  #   nCovars < 0 : model has 'nCovars' covariates but no intercept, 
  #                 e.g., dist ~ -1 + observer + height => nCovars = -2
  #   nCovars == 0 : model has (Intercept) only
  #   nCovars > 0 : model has 'nCovars' besides intercept,
  #                 e.g., dist ~ observer + height => nCovars = 2

  nCovars <- length(attr(stats::terms(res$mf), "term.labels"))
  if( attr(stats::terms(res$mf), "intercept") == 0 ){
    nCovars <- -nCovars  
  }
  res$nCovars <- nCovars
  
  # Estimated function type ----
  if( !( res$likelihood %in% c("Gamma","smu")) ){
    res$LhoodType <- "parametric" # halfnorm, hazrate, etc., all those
  } else {
    res$LhoodType <- res$likelihood  # Gamma and smu
  }

  # restore options, but make sure intEvalPts and intCoeffs match b/c 
  # they could have changed.
  options(op) # restores old intEvalPts and intCoeffs that may not match
  intCoefs <- simpsonCoefs(getOption("Rdistance_intEvalPts"))
  options(Rdistance_intCoefs = intCoefs)

  res 
}

