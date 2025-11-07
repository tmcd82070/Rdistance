#' @title Plot method for distance (detection) functions
#'   
#' @description Plot method for objects of class '\code{dfunc}'.  Objects of 
#' class '\code{dfunc}' are estimated distance functions produced by 
#' \code{\link{dfuncEstim}}. 
#'   
#' @inheritDotParams plot.dfunc.para include.zero nbins newdata 
#'     legend vertLines plotBars density angle xlab ylab 
#'     border col col.dfunc lty.dfunc lwd.dfunc 
#'   
#' @inheritParams print.dfunc 
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
#'   \item{barWidths}{A vector or scalar of the bar widths drawn on 
#'   the plot, with measurement units.  }
#'   Re-plot the bars with \code{barplot( return$barHeights, 
#'   width = return$barWidths )}.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{print.dfunc}},
#'   \code{\link{print.abund}}
#'   
#' @examples 
#' # Simulated RdistDf
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' x <- setUnits(x, "ft")
#' Df <- data.frame(transectID = "A"
#'                , distance = x
#'                 ) |> 
#'   dplyr::nest_by( transectID
#'                , .key = "detections") |> 
#'   dplyr::mutate(length = setUnits(1,"mi"))
#' attr(Df, "detectionColumn") <- "detections"
#' attr(Df, "obsType") <- "single"
#' attr(Df, "transType") <- "line"
#' attr(Df, "effortColumn") <- "length"
#' is.RdistDf(Df) # TRUE
#' 
#' dfunc <- Df |> dfuncEstim(distance ~ 1, likelihood="halfnorm")
#' plot(dfunc)
#' plot(dfunc, nbins=25)
#' 
#' # showing effects of plot parameters
#' plot(dfunc
#'   , col=c("red","blue","orange")
#'   , border="black"
#'   , xlab="Off-transect distance"
#'   , ylab="Prob"
#'   , vertLines = FALSE
#'   , main="Showing plot params")
#'  
#' plot(dfunc
#'    , col="purple"
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
#' rug(distances(dfunc))
#' 
#' # un-equal bin widths, nbins must span distances
#' plot(dfunc
#'    , nbins = c(0,2.5,5,7.5,10,15,25,50,70)
#' )
#' 
#' # Plot showing f(0)
#' hist(distances(dfunc)
#'    , n = 40
#'    , border = NA
#'    , prob = TRUE)
#' x <- seq(dfunc$w.lo, dfunc$w.hi, length=200)
#' g <- predict(dfunc, type="dfunc", distances = x, newdata = data.frame(a=1))
#' f <- g[,1] / ESW(dfunc)[1]
#' 
#' # Check integration:
#' sum(diff(x)*(f[-1] + f[-length(f)]) / 2) # Trapazoid rule; should be 1.0
#' lines(x, f) # hence, 1/f(0) = ESW
#' 
#' # Covariates: detection by observer
#' data(sparrowDfuncObserver) # pre-estimated model
#' 
#' obsLevs <- levels(sparrowDfuncObserver$data$observer)
#' plot(sparrowDfuncObserver
#'    , newdata = data.frame(observer = obsLevs)
#'    , vertLines = FALSE
#'    , col.dfunc = heat.colors(length(obsLevs))
#'    , col = c("grey","lightgrey")
#'    , border=NA
#'    , main="Detection by observer")
#' 
#' @export
plot.dfunc <- function( x
                        , ... ){

  # dispatch to other functions as follows
  is.smoothed <- Rdistance::is.smoothed(x)
  tType <- Rdistance::transectType(x)
  key <- paste0(tType, "_", is.smoothed)
  switch(key
       # , point_TRUE  = 
       # , line_TRUE   = plot.dfunc.smu(x, ...)
       , point_FALSE = 
       , line_FALSE  = plot.dfunc.para(x, ...)
       , warning( paste("Unrecognized transect type and smooth indicator. Found", key) )
  )

}
