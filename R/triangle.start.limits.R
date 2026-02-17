#' @title triangle likelihood start and limit values
#' 
#' @description Compute starting values and limits 
#' for the triangle distance function. 
#' 
#' @inheritParams startLimits
#' 
#' @inherit startLimits return
#' 
#' @seealso \code{\link{triangle.like}}
#' 
#' @examples
#' # make 'model list' object
#' # Boundary is 10, p is 100 / 120 = 0.833
#' library(Rdistance)
#' whi <- 50
#' x <- c( runif(100, min=0, max=10), runif(20, min=10, max=whi))
#' x <- setUnits(x, "m")
#' detectDf <- data.frame(transect = 1, dist = x)
#' siteDf <- data.frame(transect = 1, length = setUnits(10,"m"))
#' distDf <- RdistDf(siteDf, detectDf)
#' ml <- parseModel(distDf
#'             , formula = dist ~ 1
#'             , w.lo = 0
#'             , w.hi = setUnits(whi, "m")
#'             )
#'             
#'
#' sl <- oneStep.start.limits(ml)
#' hist(x, n = 20)
#' abline(v = exp(sl$start["(Intercept)"]))
#' 
#' 
#' @export
triangle.start.limits <- function (ml){
  
  strtVals <- oneStep.start.limits(ml)
  
  # Only difference from oneStep is theta high
  ncovars <- nCovars(stats::model.matrix(ml))
  posInf <- getOption("Rdistance_posInf")
  strtVals$high[1:ncovars] <- posInf

  strtVals
}
