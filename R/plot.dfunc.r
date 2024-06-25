#' @aliases plot.dfunc 
#'   
#' @title plot.dfunc - Plot method for distance (detection) functions
#'   
#' @description Plot method for objects of class '\code{dfunc}'.  Objects of 
#' class '\code{dfunc}' are estimated distance functions produced by 
#' \code{\link{dfuncEstim}}. 
#'   
#' @inheritDotParams plot.linePara include.zero nbins newdata 
#'     legend vertLines plotBars density angle xlab ylab 
#'     border col col.dfunc lty.dfunc lwd.dfunc 
#' 
#' @details If \code{plotBars} is TRUE, a scaled histogram is plotted
#'  and the estimated distance function
#'   is plotted over the top of it.  When bars are plotted, 
#'   this routine uses \code{graphics::barplot} 
#'  for setting up the initial plotting region and
#'  most parameters to \code{graphics::barplot} can 
#'  be specified (exceptions noted above in description of '\dots').
#'  
#'  The form of the likelihood and any series
#'   expansions is printed in the main title (overwrite this with 
#'   \code{main="<my title>"}). Convergence of the distance
#'   function is checked.  If the distance function did not converge, a warning
#'   is printed over the top of the histogram.  If one or more parameter
#'   estimates are at their limits (likely indicating non-convergence or poor
#'   fit), another warning is printed. 
#'   
#'   
#' @return The input distance function is returned, with two additional
#'   components than can be used to reconstruct the plotted bars.  (To 
#'   obtain values of the plotted distance functions, use \code{predict}
#'   with \code{type = "distances"}.) 
#'   The additional components are:
#'   \item{barHeights}{A vector containing the scaled bar heights drawn 
#'   on the plot.}
#'   \item{barWidths}{A vector or scaler of the bar widths drawn on 
#'   the plot, with measurement units.  }
#'   Re-plot the bars with \code{barplot( return$barHeights, 
#'   width = return$barWidths )}.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{print.dfunc}},
#'   \code{\link{print.abund}}
#'   
#' @examples 
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' x <- units::set_units(x, "ft")
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc)
#' plot(dfunc, nbins=25)
#' 
#' # showing effects of plot params
#' plot(dfunc
#'   , col=c("red","blue","orange")
#'   , border="black"
#'   , xlab="Off-transect distance"
#'   , ylab="Prob"
#'   , vertLines = FALSE
#'   , main="Showing plot params")
#'  
#' plot(dfunc
#'    , col="wheat"
#'    , density=30
#'    , angle=c(-45,0,45)
#'    , cex.axis=1.5
#'    , cex.lab=2
#'    , ylab="Probability") 
#' 
#' plot(dfunc
#'    , col=c("grey","lightgrey")
#'    , border=NA) 
#' 
#' plot(dfunc
#'    , col="grey"
#'    , border=0
#'    , col.dfunc="blue"
#'    , lty.dfunc=2
#'    , lwd.dfunc=4
#'    , vertLines=FALSE)
#' 
#' plot(dfunc
#'    , plotBars=FALSE
#'    , cex.axis=1.5
#'    , col.axis="blue")
#' rug(dfunc$detections$dist)
#' 
#' # Plot showing f(0)
#' hist(dfunc$detections$dist
#'    , n = 40
#'    , border = NA
#'    , prob = TRUE)
#' x <- seq(dfunc$w.lo, dfunc$w.hi, length=200)
#' y <- predict(dfunc, type="dfunc", distances = x)
#' lines(x, c(y)/attr(y, "scaler"))
#' c(attr(y,"scaler") / y[1], ESW(dfunc))  # 1/f(0) = ESW
#' 
#' # Covariates: detection by observer
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' dfuncObs <- dfuncEstim(formula = dist ~ observer + groupsize(groupsize)
#'                      , likelihood = "hazrate"
#'                      , detectionData = sparrowDetectionData
#'                      , siteData = sparrowSiteData)
#' plot(dfuncObs
#'    , newdata = data.frame(observer = levels(sparrowSiteData$observer))
#'    , vertLines = FALSE
#'    , lty = c(1,1)
#'    , col.dfunc = heat.colors(length(levels(sparrowSiteData$observer)))
#'    , col = c("grey","lightgrey")
#'    , border=NA
#'    , main="Detection by observer")
#' 
#' @keywords models
#' @export
#' @importFrom graphics hist barplot axTicks 
#' @importFrom graphics axis plot title lines text
#' @importFrom grDevices rainbow

plot.dfunc <- function( x
                        , ... ){

  # dispatch to other functions as follows
  is.smoothed <- Rdistance::is.smoothed(x)
  key <- paste0(x$transType, "_", is.smoothed)
  switch(key
       , line_TRUE   = plot.lineSmu(x, ...)
       , point_TRUE  = plot.pointSmu(x, ...)
       , line_FALSE  = plot.linePara(x, ...)
       , point_FALSE = plot.pointPara(x, ...)
       , warning( paste("Unrecognized transect type and smooth indicator. Found", key) )
  )

}
