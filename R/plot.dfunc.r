#' @aliases plot.dfunc
#'   
#' @title plot.dfunc - Plot method for distance (detection) functions
#'   
#' @description Plot method for objects of class '\code{dfunc}'.  Objects of 
#' class '\code{dfunc}' are estimated distance functions produced by 
#' \code{\link{dfuncEstim}}. 
#'   
#' @param x An estimated distance function resulting from a call to
#'   \code{dfuncEstim}.
#'   
#' @param include.zero Boolean value specifying whether to include 0 on the x-axis 
#' of the plot.  A value of TRUE will include 0 on the left hand end of the x-axis
#' regardless of the range of distances.  A value of FALSE will plot only the
#' input distance range (\code{w.lo} to \code{w.hi}).
#'   
#' @param nbins Internally, this function uses \code{hist} to compute histogram
#'   bars for the plot. This argument is the \code{breaks} argument to
#'   \code{hist}.  This can be either a vector giving the breakpoints between
#'   bars, the suggested number of bars (a single number), a string naming
#'   an algorithm to compute the number of bars, or a function to compute the
#'   number of bars.  See \code{\link{hist}} for all options.
#'   
#' @param newdata Data frame similar to the \code{newdata} parameter 
#' to \code{\link{lm}} containing new values for covariates in the distance function.
#' One distance function is computed and plotted for each row in the data frame. 
#' If \code{newdata} is NULL, the routine computes the mean of all numeric covariates
#' in the distance function and the mode of all factor covariates in 
#' the distance function. The new mean and mode vector is used to predict 
#' and plot a distance function.  
#'   
#' @param legend Logical scalar for whether to include a legend. 
#'   If TRUE, a legend will be included on the plot detailing
#'   the covariate values use to generate the plot.
#'   
#' @param plotBars Logical scalar for whether to plot the histogram 
#' of distances behind the distance function.  If FALSE, no histogram 
#' is plotted, only the distance function line(s).
#'   
#' @param xlab Label for the x-axis
#' 
#' @param ylab Label for the y-axis
#' 
#' @param density If \code{plotBars=TRUE}, a vector giving the density of 
#' shading lines, in lines per inch, for the bars underneath 
#' the distance function, repeated as necessary to exceed the number 
#' of bars. Values of NULL or a number strictly less than 0 
#' mean solid fill using colors from parameter \code{col}. 
#' If \code{density = 0}, bars are not filled and only the borders are rendered. 
#' If \code{density} >0, bars are shaded with colors and angles from 
#' parameters \code{col} and \code{angle}.
#' 
#' @param angle When \code{density} is >0, the slope of bar shading lines, 
#' given as an angle in degrees (counter-clockwise), repeated as necessary
#' to exceed the number of bars.
#' 
#' @param col A vector of bar fill colors or line colors when bars are 
#' drawn and \code{density != 0}, repeated as necessary to exceed the number
#' of bars. Also used for the bar borders when
#' \code{border = TRUE}.
#'  
#' @param border The color of bar borders when bars are plotted, 
#' repeated as necessary to exceed the number of bars. A 
#' value of NA or FALSE means no borders. If bars are shaded with lines 
#' (i.e., \code{density>0}), \code{border = TRUE} uses the same 
#' color for the border as for the shading lines.  Otherwise, fill color
#' or shaded line color are specified in \code{col} while 
#' border color is specified in \code{border}.  
#' 
#' @param vertLines Logical scalar specifying whether to plot vertical 
#'  lines at \code{w.lo} and \code{w.hi} from 0 to the  
#'  distance function. 
#'  
#' @param col.dfunc Color of the distance function(s).
#' If only one distance function (one line) is being plotted, 
#' the default color is "red".
#' If covariates or \code{newdata} are present, 
#' the default value uses \code{graphics::rainbow(n)}, 
#' where \code{n} is the number 
#' of plotted distance functions.  Otherwise, \code{col.dfunc} 
#' is replicated to the required length.  Plot all 
#' distance functions in the same color by setting 
#' \code{col.dfunc} to
#' a scalar. Plot blue-red pairs of distance functions 
#' by setting \code{col.dfunc} = \code{c("blue", "red")}. Etc. 
#'   
#' @param lty.dfunc Line type of the distance function(s).
#' If covariates or \code{newdata} is present, 
#' the default uses line types  
#' to \code{1:n}, where \code{n} is the number 
#' of plotted distance functions.  Otherwise, \code{lty.dfunc} 
#' is replicated to the required length. Plot solid lines
#' by specifying \code{lty.dfunc = 1}. Plot solid-dashed line pairs
#' by specifying \code{lty.dfunc = c(1,2)}. Etc.
#' 
#' @param lwd.dfunc Line width of the distance function(s), replicated 
#' to the required length. Default is 2 for all lines.  
#' 
#' @param \dots When bars are plotted, this routine 
#'  uses \code{graphics::barplot} to set up the 
#'  plotting region and plot bars. When bars are not plotted,
#'  this routine sets up the plot with \code{graphics::plot}.
#'  \dots can be any other 
#'  argument to \code{barplot} or \code{plot} EXCEPT  
#'  \code{width}, \code{ylim}, \code{xlim}, 
#'  \code{density}, \code{angle}, and \code{space}.
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
#'   components than can be used to reconstruct the plotted bars.  To 
#'   obtain values of the plotted distance functions, use \code{predict}
#'   with \code{type = "distances"}. 
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

plot.dfunc <- function( x, 
                        include.zero=FALSE, 
                        nbins="Sturges", 
                        newdata = NULL, 
                        legend = TRUE, 
                        vertLines=TRUE,
                        plotBars=TRUE,
                        density = -1,
                        angle = 45,
                        xlab = NULL,
                        ylab = NULL,
                        border = TRUE,
                        col = "grey85",
                        col.dfunc=NULL,
                        lty.dfunc=NULL,
                        lwd.dfunc=NULL,
                        ... ){

  # a constant used later
  zero <- units::as_units(0, x$outputUnits)
  
  is.smoothed <- class(x$fit) == "density"
  d <- x$detections$dist
  whi <- x$w.hi
  wlo <- x$w.lo
  xInStrip <- d[(d < whi) & (d > wlo)]
  cnts <- hist( xInStrip
             , plot=FALSE
             , breaks=nbins
             , warn.unused = FALSE )
  
  # hist should return breaks with units attached, but it does not
  cnts$breaks <- units::as_units(cnts$breaks, x$outputUnits)
  cnts$mids <- units::as_units(cnts$mids, x$outputUnits)
  xscl <- cnts$mid[2] - cnts$mid[1]

  #   Gotta add bars on the left if first bar is not at w.lo.  I.e., if first 
  #   bar is not zero.  Zero bars at top end are not a problem, but low end are because
  #   barplot just plots bars, not coordinates
  if( cnts$breaks[1] > x$w.lo ){
    # do the hist again, this time specifying breaks exactly
    brks <- seq(x$w.lo, x$w.hi, by=xscl)
    brks <- c(brks, brks[length(brks)] + xscl )   # make sure last bin goes outside range of data
    cnts <- hist( xInStrip
                  , plot=FALSE
                  , breaks=units::drop_units(brks)
                  , include.lowest=TRUE
                  , warn.unused = FALSE)
    cnts$breaks <- units::as_units(cnts$breaks, x$outputUnits)
    cnts$mids <- units::as_units(cnts$mids, x$outputUnits)
  }

  like <- match.fun( paste( x$like.form, ".like", sep=""))
  
  x.seq <- seq( x$w.lo, x$w.hi, length=200)
  
  # Work out covariates to use when missing ----
  

  # Fixup new data ----
  if((missing(newdata) || is.null(newdata)) & !is.smoothed ){
    
    # Function returning mode (=most frequent values) of a FACTOR.
    # only needed in this case.
    getmode <- function(v) {
      uniqv <- table(v)
      factor(names(uniqv)[which.max(uniqv)], levels = levels(v))
    }
    
    # Note: x$model.frame has all distances, even those < w.lo and > w.hi,
    # and x$model.frame[,attr(terms(x$model.frame), "response")] is the response (i.e., distances, usually column 1).
    # x$covars has ONLY covariates for distances between w.lo and w.hi
    # x$model.frame has the original factors un-expanded to indicator variables.
    # x$covars has all factors expanded into indicators using specified contrasts.

    covNames <- labels(terms(x$model.frame)) # Intercept not included here
    newdata <- matrix(NA, nrow = 1, ncol = length(covNames))
    colnames(newdata) <- covNames
    newdata <- data.frame(newdata)
    # need to eventually filter model.frame. x$detections$dist has wrong number of rows
    # b/c obs outside strip are not there.
    origDist <- model.response(x$model.frame) 
    inStrip <- (x$w.lo <= origDist) & (origDist <= x$w.hi)
    factor.names <- attr(terms(x$model.frame), "dataClasses")
    factor.names <- names(factor.names)[ factor.names == "factor" ]
    for( nm in covNames ) {
      if( nm %in% factor.names ) {
        newdata[,nm] <- getmode(x$model.frame[inStrip, nm]) # Use mode to predict
      } else {
        newdata[,nm] <- mean(x$model.frame[inStrip, nm]) # Use mean
      }
    }
  }

  if(is.null(x$covars)){
    # If model has no covariates, there is only one value of params regardless of newdata.
    newdata <- newdata[1,,drop = FALSE]
  }
  
  # Predict distance functions ----
  y <- stats::predict(object = x
                    , newdata = newdata
                    , distances = x.seq
                    , type = "distances"
                    )

  if( x$pointSurvey ){
    y <- y * units::drop_units(x.seq - x$w.lo)
    scaler <- units::drop_units(x.seq[2]-x.seq[1]) * colSums(y[-nrow(y),,drop = FALSE]+y[-1,,drop = FALSE]) / 2
  } else {
    # line transects here
    scaler <- attr(y, "scaler")
  }
  ybarhgts <-  cnts$density * mean(scaler)

  # after here, y is a matrix, columns are distance functions.
  
  if( include.zero & x$like.form == "hazrate" ){
    x.seq[1] <- x$w.lo
  }
  
  y.finite <- y[ y < Inf ]
  y.lims <- c(0, max( x$g.x.scl, ybarhgts, y.finite, na.rm=TRUE ))
  
  if( include.zero ){
    x.limits <- c(zero , max(x.seq))
  } else {
    x.limits <- range(x.seq) 
  }

  # Default xlab ----
  if(is.null(xlab)){
    xlab <- paste0("Distance ", format(zero)) # zero cause it has units
    xlab <- sub("0 ", "", xlab) # erase 0 but leave units
  } 
  
  # Default ylab ----
  if(is.null(ylab)){
    ylab <- if(x$pointSurvey) "Observation density" else "Probability of detection"
  }
  
  # Main plot ----
  if(plotBars){
    if(x$w.lo != zero){
      ybarhgts <- c(NA,ybarhgts)
      xscl <- c(x$w.lo, rep(xscl,length(ybarhgts)-1))
      # the following deal with density is because barplot draws the border
      # of the NA box when density >= 0.  Makes no sense, but there it is.
      # This produces a line to 0 when w.lo > 0
      if( any(density > 0) ){
        warning("'density' of bars cannot be positive when w.lo > 0. Set to 0 or negative.")
        density[ density > 0 ] <- -1
      }
      if( any(density == 0) ){
        # bars are supposed to be empty when density = 0, so reset to fill with background
        col[density == 0] = par()$bg
        density[density == 0] = -1
      }
    }
    bar.mids <- barplot( ybarhgts, 
                         width = xscl, 
                         ylim = y.lims, 
                         xlim = x.limits,
                         space = 0, 
                         density = density,
                         angle = angle,
                         col = col,
                         border = border,
                         xlab = xlab,
                         ylab = ylab,
                         ... )  
    xticks <- axTicks(1)
    axis( 1, at=xticks,  labels=xticks, line=.5, ... )
  } else {
    plot(1,1,type="n",
         ylim = y.lims, 
         xlim = x.limits, 
         xlab = xlab,
         ylab = ylab,
         bty = "n",
         ...)
  }

  
  # Default main ----
  if( !("main" %in% names(list(...))) ){
    # Put up a default title containing likelihood description
    if( x$like.form == "smu" ){
      title(main=paste( x$fit$call[["kernel"]], "kernel smooth"))
      mtext(paste0("Bandwidth ", x$fit$call[["bw"]], 
                       "; Adjust ", format(x$fit$call[["adjust"]])), 
            side=3, cex=.75, line=0.8)
    } else if( x$expansions == 0 ){
      title(main=paste( x$like.form, sep=""))
    } else {
      if( x$expansions == 1 ){
        expText <- "expansion"
      } else {
        expText <- "expansions"
      }
      title(main=paste0( x$like.form, ", ", x$expansions, " ", x$series, " ", expText, sep=""))
    }
  }
  
  #   These 3 lines plot a polygon for the density function
  #x.poly <- c(0, x, x[length(x)] )
  #y.poly <- c(0, y, 0)
  #polygon( x.poly, y.poly, density=15, border="red", lwd=2 )
  
  #   Work out the colors, line types, and line widths ----
  if( is.null(col.dfunc) ){
    # rainbow(1) is red, the default for one line
    col.dfunc <- rainbow(ncol(y))
  } else if(length(col.dfunc) < ncol(y)){
    col.dfunc <- rep(col.dfunc,ceiling(ncol(y)/length(col.dfunc)))[1:ncol(y)]
  }
  if( is.null(lty.dfunc) ){
    lty.dfunc <- 1:ncol(y)
  } else if(length(lty.dfunc) < ncol(y)){
    lty.dfunc <- rep(lty.dfunc,ceiling(ncol(y)/length(lty.dfunc)))[1:ncol(y)]
  }
  if( is.null(lwd.dfunc) ){
    lwd.dfunc <- rep(2,ncol(y))
  } else if(length(lwd.dfunc) < ncol(y)){
    lwd.dfunc <- rep(lwd.dfunc,ceiling(ncol(y)/length(lwd.dfunc)))[1:ncol(y)]
  }
  
  # Draw distance functions ----
  for(i in 1:ncol(y)){
    lines( x.seq, y[,i]
         , col=col.dfunc[i]
         , lwd=lwd.dfunc[i]
         , lty = lty.dfunc[i]
         , xpd = TRUE
         )
  }

  #   Add vertical lines at 0 and w if called for ----
  if(vertLines){
    lines( rep(x.seq[1], 2), c(0,y[1]), col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1])
    lines( rep(x.seq[length(x.seq)], 2), c(0,y[length(x.seq)]), 
           col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1] )
  }
  
  # Check convergence ----
  if( (x$like.form != "smu") && x$convergence != 0 ){
    if( x$convergence == -1 ){
      mess <- "Solution failure"
    } else {
      mess <- "Convergence failure"
    }
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", x$fit$message, sep=""), cex=1, adj=.5, col="black")
  }
  
  # Check for Probabilities > 1 ----
  if( any(y > 1) & (!x$pointSurvey) ){
    # some g(x) > 1, should'nt happen
    mess <- "Probabilities > 1"
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- "g(x) > 1 should not happen"
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
    mess <- paste0("Check g(", format(x$x.scl), ")= ", round(x$g.x.scl,2), " assumption")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  } 
  
  # Check missing or <0 y values ----
  if( any(is.na(y)) | any(y < 0) ){
    #   invalid scaling, g0 is wrong
    mess <- "Probabilities < 0"
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- paste("One or more parameters likely at boundary")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  }

  # Legend ----
  if(legend & !is.null(x$covars)){
    for(j in 1:ncol(newdata)){
      if(is.numeric(newdata[,j])){
        newdata[,j]<-signif(newdata[,j], 3)
      }
    }
    nr <- nrow(newdata)
    v.names <- names(newdata)
    v.names <- rep(v.names, each=nr)
    v.vals <- c(as.matrix(newdata))
    leg <- matrix( paste(v.names, v.vals,sep="="), nr)
    Leg <- leg[,1]
    if( ncol(leg) >= 2){
      for(j in 2:ncol(leg)){
        Leg <- paste(Leg, leg[,j], sep=",") 
      }
    }
    
    
    #legend.factors <- vector(length = length(x$legend.names))

    legend('topright', legend = Leg, lty = lty.dfunc, lwd = lwd.dfunc, 
           col = col.dfunc, cex = 0.7)
  }
  
  # Clean up ----
  # x$yscl <- scaler   # this is g(x) / f(x) = ESW if lines. One for each row in newdata.  Might want this later.
  x$barHeights <- ybarhgts  # scaled to mean scalar.
  x$barWidths <- xscl
  x$predCovValues <- newdata
  
  invisible(x)
}
