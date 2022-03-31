#' @aliases plot.dfunc
#'   
#' @title Plot a distance (detection) function
#'   
#' @description Plot method for an estimated distance function. Estimated
#'   distance functions are of class 'dfunc'
#'   
#' @param x An estimated distance function resulting from a call to
#'   \code{dfuncEstim}.
#'   
#' @param include.zero Boolean value specifying whether to include 0 in the
#'   plot.  A value of TRUE will include 0 on the left hand end of the x-axis
#'   regardless of the range of distances.  A value of FALSE will plot only the
#'   input distance range (\code{w.lo} to \code{w.hi}).
#'   
#' @param nbins Internally, this function uses \code{hist} to compute histogram
#'   bars for the plot. This argument is the \code{breaks} argument to
#'   \code{hist}.  This can be either a vector giving the breakpoints between
#'   bars, a single number giving the suggested number of bars, a string naming
#'   an algorithm to compute the number of bars, or a function to compute the
#'   number of bars.  See \code{help(hist)} for all options.
#'   
#' @param newdata Matrix containing values of covariates to plot. Each row is a
#'   set of covariate values (i.e. each column contains all values of each
#'   covariate)
#'   
#' @param legend Logical scalar for whether to include legend. 
#'   If TRUE, a legend will be included on plot detailing
#'   covariate values plotted.
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
#' the distance function. Values of NULL or a number strictly less than 0 
#' mean solid fill using colors from parameter \code{col}.  If 
#' \code{density =0 }, bars are not filled with any color or lines. 
#' 
#' @param col A vector of bar fill colors or line colors when bars are 
#' drawn and \code{density != 0}, replicated
#' to the correct length. A value of 0 is the background color.
#' 
#' @param border The color of bar borders when bars are plotted. A 
#' value of NA means no borders. If there are shading lines 
#' (i.e., \code{density>0}), \code{border = TRUE} uses the same 
#' color for the border as for the shading lines.
#' 
#' @param vertLines Logical scalar specifying whether to plot vertical 
#'  lines at \code{w.lo} and \code{w.hi} from 0 to the  
#'  distance function. 
#'  
#' @param col.dfunc Color of the distance function(s), replicated to 
#' the required length. If covariates or \code{newdata} is present 
#' and \code{length(col.dfunc)==1}, 
#' \code{col.dfunc} is expanded to 
#' to number of plotted distance functions by setting it equal 
#' to \code{graphics::rainbow(n)}, where \code{n} is the number 
#' of plotted distance functions.  If you want to plot all 
#' distance functions in the same color, set \code{col.dfunc} to
#' a constant vector having length at least 2 (e.g., 
#' \code{col.dfunc = c(1,1)}) will 
#' plot all curves in black).
#'   
#'  
#' @param lty.dfunc Line type of the distance function(s), replicated 
#' to the required length.  If covariates or \code{newdata} is present 
#' and \code{length(lty.dfunc)==1}, 
#' \code{lty.dfunc} is expanded to 
#' to number of plotted distance functions by setting it equal 
#' to \code{lty.dfunc + 0:(n-1)}, where \code{n} is the number 
#' of plotted distance functions.  If you want to plot all 
#' distance functions using the same line type, set \code{lty.dfunc} to
#' a constant vector having length at least 2 (e.g., 
#' \code{lty.dfunc = c(1,1)}) will 
#' plot all solid lines).
#' 
#' @param lwd.dfunc Line width of the distance function(s), replicated 
#' to the required length.  
#' 
#' @param \dots When bars are plotted, this routine 
#'  uses \code{graphics::barplot} for setting up the 
#'  plotting region and plotting bars. When bars are not plotted,
#'  this routine sets up the plot with \code{graphics::plot}.
#'  \dots can be any other 
#'  argument to \code{barplot} or \code{plot} EXCEPT  
#'  \code{width}, \code{ylim}, \code{xlim}, and \code{space}.
#'   
#' @details If \code{plotBars} is TRUE, a scaled histogram is plotted
#'  and the estimated distance function
#'   is plotted over the top of it.  When bars are plotted, 
#'   this routine uses \code{graphics::barplot} 
#'  for setting up the initial plotting region and
#'  most parameters to \code{graphics::barplot} can 
#'  be specified (exceptions noted above in description of '\dots').The form of the likelihood and any series
#'   expansions is printed in the main title (overwrite this with 
#'   \code{main="<my title>"}). Convergence of the distance
#'   function is checked.  If the distance function did not converge, a warning
#'   is printed over the top of the histogram.  If one or more parameter
#'   estimates are at their limits (likely indicating non-convergence or poor
#'   fit), another warning is printed. 
#'   
#'   
#' @return The input distance function is returned, with two additional
#'   components related to the plot that may be needed if additional lines or
#'   text is to added to the plot by the user.  These additional components are:
#'   
#'   \item{xscl.plot}{Scaling factor for horizontal coordinates.  Due to the way
#'   \code{barplot} works, the x-axis has been scaled.  The internal coordinates
#'   of the bars are 1, 2, \ldots, \code{nbars}. To plot something at a distance
#'   coordinate of x, x must be divided by this value.  For example, to draw a
#'   vertical line at a value of 10 on the x-axis, the correct call is
#'   \code{abline(v=10/obj$xscl.plot)}.  }
#'   
#'   \item{yscl}{Scaling factor for vertical coordinates.  The histogram and
#'   distance function plotted by this routine are scaled so that height of the
#'   distance function at \code{w.lo} is \code{g0}.  Usually, this means the
#'   distance curve is scaled so that the y-intercept is 1, or that g(0) = 1. 
#'   To add a plot feature at a real coordinate of y, y must be divided by this
#'   returned parameters.  For example, to draw a horizontal line at y-axis
#'   coordinate of 1.0, issue \code{abline(h=1/obj$yscl)}.  }
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr 
#' Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{print.dfunc}},
#'   \code{\link{print.abund}}
#'   
#' @examples 
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc)
#' plot(dfunc, nbins=25)
#' 
#' # showing effects of plot params
#' plot(dfunc, col=c("red","blue","orange"), 
#'  border="black", xlab="Dist (m)", ylab="Prob", 
#'  vertLines = FALSE, main="Showing plot params")
#'  
#' plot(dfunc, col="wheat", density=30, angle=c(-45,0,45), 
#' cex.axis=1.5, cex.lab=2, ylab="Probability") 
#' 
#' plot(dfunc, col=c("grey","lightgrey"), border=NA) 
#' 
#' plot(dfunc, col="grey", border=0, col.dfunc="blue", 
#' lty.dfunc = 2, lwd.dfunc=4, vertLines=FALSE)
#' 
#' plot(dfunc, plotBars=FALSE, cex.axis=1.5, col.axis="blue") 
#' rug(dfunc$dist)
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
                        density = NULL, 
                        xlab = NULL,
                        ylab = NULL,
                        border = "blue",
                        col = 0,
                        col.dfunc="red",
                        lty.dfunc=1,
                        lwd.dfunc=2,
                        ... ){

  # a constant used later
  zero <- units::as_units(0, x$outputUnits)
  
  cnts <- hist( x$dist[x$dist<x$w.hi & x$dist>x$w.lo], plot=FALSE, 
                breaks=nbins, warn.unused = FALSE )
  
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
    cnts <- hist( x$dist[x$dist<x$w.hi & x$dist>x$w.lo], plot=FALSE, 
                  breaks=brks, include.lowest=TRUE, 
                  warn.unused = FALSE)
    cnts$breaks <- units::as_units(cnts$breaks, x$outputUnits)
    cnts$mids <- units::as_units(cnts$mids, x$outputUnits)
  }

  # Figure out scaling
  if( is.null( x$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- zero
    warning("g0 unspecified.  Assumed 1.")
  } else {
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
  }
  
  # Create the function that calculates mode (=most frequent values) of a vector. 
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  like <- match.fun( paste( x$like.form, ".like", sep=""))
  
  x.seq <- seq( x$w.lo, x$w.hi, length=200)
  
  if(!is.null(x$covars)){
    if(!is.null(x$factor.names)) {
      fac <- x$model.frame[,x$factor.names]
    } 

    covMeans <- matrix(NA, nrow = 1, ncol = length(colnames(x$model.frame[-c(1)])))
    colnames(covMeans) <- colnames(x$model.frame[-c(1)])
    for(n in colnames(x$model.frame[-c(1)])) {
      if(n %in% x$factor.names) {
        modeFac <- getmode(x$model.frame[[n]])# Calculate mode
        covMeans[,n] <- modeFac # store
      } else {
        meanFac <- mean(x$model.frame[[n]])# Calculate mean
        covMeans[,n] <- meanFac # store
      }
    }
    
    # compute column means of covariantes because need them later to scale bars
    covMeanMat <- col.m <- colMeans(x$covars)
    
    covMeanMat <- matrix(covMeanMat, 1) # this has the intercept
    
    if(missing(newdata) || is.null(newdata)){
      newdata <- as.data.frame(covMeans)
    }
    
    params <- predict.dfunc(x, newdata, type="parameters")
    
    # Use covars= NULL here because we evaluated covariates to get params above
    # after apply, y is length(x) x nrow(newdata).  each column is a unscaled distance 
    # function (f(x))
    y <- apply(params, 1, like, dist= x.seq - x$w.lo, 
               series=x$series, covars = NULL, 
               expansions=x$expansions, 
               w.lo = x$w.lo, w.hi=x$w.hi, 
               pointSurvey = FALSE )  
    y <- t(y)  # now, each row of y is a dfunc
    
    f.at.x0 <- apply(params, 1, like, dist= x0 - x$w.lo, 
                     series=x$series, covars = NULL, 
                     expansions=x$expansions, 
                     w.lo=x$w.lo, w.hi=x$w.hi, 
                     pointSurvey = FALSE )
    scaler <- g.at.x0 / f.at.x0 # a length n vector 
    
    y <- y * scaler  # length(scalar) == nrow(y), so this works right

    y <- t(y)
        
    if(x$pointSurvey){
      y <- y * (x.seq - x$w.lo)
    }
  }  else {
    y <- like( x$parameters, x.seq - x$w.lo, series=x$series, expansions=x$expansions, 
               w.lo=zero, w.hi=x$w.hi - x$w.lo, pointSurvey = FALSE, covars=NULL )

    if(x$pointSurvey){
      f.at.x0 <- like( x$parameters, x0 - x$w.lo, series=x$series, expansions=x$expansions, 
                       w.lo=zero, w.hi=x$w.hi-x$w.lo, pointSurvey = FALSE, covars=NULL, 
                       scale=FALSE)
      
      if(any(is.na(f.at.x0) | (f.at.x0 <= 0))){
        #   can happen when parameters at the border of parameter space
        scaler <- 1
        warning("g(x.scale) is missing or <= zero. One or more parameters likely at their boundaries. Caution.")
      } else {
        scaler <- g.at.x0 / f.at.x0 
      }
      
      y <- y * scaler  
      y <- y * units::drop_units(x.seq - x$w.lo)
    }
  }
  
  if( include.zero & x$like.form == "hazrate" ){
    x.seq[1] <- x$w.lo
  }
  

  if(!is.null(x$covars)){
    if( !x$pointSurvey ){
      f.max <- F.maximize.g(x, covMeanMat)
      yscl <- g.at.x0 / f.max
      #yscl <- 1
      if(length(yscl > 1)){
        yscl <- yscl[1]
      }
      ybarhgts <- cnts$density * yscl
      # plotBars <- TRUE
    } else {
      yscl <- (x.seq[2]-x.seq[1]) * sum(y[-length(y)]+y[-1]) / 2
      ybarhgts <-  cnts$density * yscl
      # plotBars <- FALSE
    }
  } else {
    if( !x$pointSurvey ){
      # (tlm) someone stuck in the following line, which works when xmax = 0, 
      # but I don't think works for other cases, like Gamma.
      #f.max <- F.maximize.g(x, covars = NULL) 
      
      f.max <- like( x$parameters, x0 - x$w.lo, series=x$series, covars=NULL,
                     expansions=x$expansions, w.lo=zero, 
                     w.hi=x$w.hi-x$w.lo, pointSurvey = x$pointSurvey )

      if(any(is.na(f.max) | (f.max <= 0))){
        #   can happen when parameters at the border of parameter space
        yscl <- 1.0
        warning("g(x.scale) is missing or <= zero. One or more parameters likely at their boundaries. Caution.")
      } else {
        yscl <- g.at.x0 / f.max
      }

      if(length(yscl > 1)){
        yscl <- yscl[1]
      }
      y <- y * yscl
      ybarhgts <- cnts$density * yscl
    } else {
      yscl <- units::drop_units(x.seq[2]-x.seq[1]) * sum(y[-length(y)]+y[-1]) / 2
      ybarhgts <-  cnts$density * yscl
    }
  }
  
  y.finite <- y[ y < Inf ]
  y.lims <- c(0, max( g.at.x0, ybarhgts, y.finite, na.rm=TRUE ))
  
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
    }
    bar.mids <- barplot( ybarhgts, 
                         width = xscl, 
                         ylim = y.lims, 
                         xlim = x.limits,
                         space = 0, 
                         density = density,
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
      title(main=paste( x$like.form, ", ", x$expansions, " expansions", sep=""))
    } else {
      title(main=paste( x$like.form, ", ", x$series, " expansion, ", x$expansions, " expansions", sep=""))
    }
  }
  
  #   These 3 lines plot a polygon for the density function
  #x.poly <- c(0, x, x[length(x)] )
  #y.poly <- c(0, y, 0)
  #polygon( x.poly, y.poly, density=15, border="red", lwd=2 )
  
  #   Draw the distance function lines
  if(is.matrix(y)){
    if(length(col.dfunc) == 1){
      col.dfunc <- rainbow(ncol(y))
    } else if(length(col.dfunc) < ncol(y)){
      col.dfunc <- rep(col.dfunc,ceiling(ncol(y)/length(col.dfunc)))[1:ncol(y)]
    }
    if(length(lty.dfunc) == 1){
      lty.dfunc <- lty.dfunc + 0:(ncol(y)-1)
    } else if(length(lty.dfunc) < ncol(y)){
      lty.dfunc <- rep(lty.dfunc,ceiling(ncol(y)/length(lty.dfunc)))[1:ncol(y)]
    }
    if(length(lwd.dfunc) == 1){
      lwd.dfunc <- rep(lwd.dfunc,ncol(y))
    } else if(length(lwd.dfunc) < ncol(y)){
      lwd.dfunc <- rep(lwd.dfunc,ceiling(ncol(y)/length(lwd.dfunc)))[1:ncol(y)]
    }
    for(i in 1:ncol(y)){
      lines( x.seq, y[,i], col=col.dfunc[i], lwd=lwd.dfunc[i], lty = lty.dfunc[i] )
    }
  }
  else{
    lines( x.seq, y, col=col.dfunc, lwd=lwd.dfunc, lty=lty.dfunc )
  }


  #   These two add vertical lines at 0 and w if called for
  if(vertLines){
    lines( rep(x.seq[1], 2), c(0,y[1]), col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1])
    lines( rep(x.seq[length(x.seq)], 2), c(0,y[length(x.seq)]), 
           col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1] )
  }
  
  #   print area under the curve
  area <- effectiveDistance(x)
  #area2 <- (x[3] - x[2]) * sum(y[-length(y)]+y[-1]) / 2   # use x[3] and x[2] because for hazard rate, x[1] is not evenly spaced with rest
  #print(c(area,area2))
  #text( max(x.seq), max(y.lims)-0.025*diff(y.lims), paste("ESW =", round(area,3)), adj=1)
  
  #   If model did not converge, print a message on the graph.
  if( (x$like.form != "smu") && x$convergence != 0 ){
    if( x$convergence == -1 ){
      mess <- "Solution failure"
    } else {
      mess <- "Convergence failure"
    }
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", x$fit$message, sep=""), cex=1, adj=.5, col="black")
  } else if( any(is.na(area)) | any(area > x$w.hi) ){
    #   invalid scaling, g0 is wrong
    mess <- "Scaling failure(s)"
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- paste("Check g0=", round(g.at.x0,2), "assumption")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  } else if( any(is.na(y)) | any(y < 0) ){
    #   invalid scaling, g0 is wrong
    mess <- "Probabilities < 0"
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- paste("One or more parameters likely at boundary")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  }

  # Add legend to plot if covars are present
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
  
  #x$xscl.plot <- xscl   # gonna need this to plot something on the graph.
  x$yscl <- yscl   # this is g(x) / f(x).  Might want this later.
  
  invisible(x)
}
