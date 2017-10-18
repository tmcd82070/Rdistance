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
#'   range on input distanced.
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
#' @param legend Boolean. If TRUE, a legend will be included on plot detailing
#'   covariate values plotted.
#'   
#' @param \dots Other arguments to \code{barplot}, such as \code{cex},
#'   \code{col}, \code{bty}, etc. The following plot parameters cannot be
#'   included in \dots: \code{space}, \code{density}, \code{ylim}, \code{xlim},
#'   and \code{border}.  In addition, \code{main}, \code{ylab}, and \code{xlab}
#'   should not be used because the internal values will overwrite whatever
#'   values are given.
#'   
#' @details A scaled histogram is plotted, and the estimated distance function
#'   is plotted over the top of it.  The form of the likelihood and any series
#'   expansions is printed in the main title (overwrite this with 
#'   \code{main="<my title>"}). Convergence of the distance
#'   function is checked.  If the distance funtion did not converge, a warning
#'   is printed over the top of the histogram.  If one or more parameter
#'   estimates are at their limits (likely indicating non-covergence or poor
#'   fit), another warning is printed. 
#'   
#' @return The input distance function is returned, with two additional
#'   components related to the plot that may be needed if additional lines or
#'   text is to added to the plot by the user.  These additional components are:
#'   
#'   \item{xscl.plot}{Scaling factor for horizontal coordinates.  Due to the way
#'   \code{barplot} works, the x-axis has been scaled.  The internal coordinates
#'   of the bars are 1, 2, \ldots, nbars. To plot something at a distance
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
#' @examples \dontrun{
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc)
#' plot(dfunc, nbins=25)
#' }
#' @keywords models
#' @export
#' @importFrom graphics hist barplot axTicks axis plot title lines text

plot.dfunc <- function( x, include.zero=FALSE, nbins="Sturges", 
                        newdata = NULL, legend = TRUE, ... ){


  cnts <- hist( x$dist[x$dist<x$w.hi & x$dist>x$w.lo], plot=FALSE, breaks=nbins )
  xscl <- cnts$mid[2] - cnts$mid[1]


  #   Gotta add bars on the left if first bar is not at w.lo.  I.e., if first 
  #   bar is zero.  Zero bars at top end are not a problem, but low end are because
  #   barplot just plots bars, not coordinates
  if( cnts$breaks[1] > x$w.lo ){
    # do the hist again, this time specifying breaks exactly
    brks <- seq(x$w.lo, x$w.hi, by=xscl)
    brks <- c(brks, brks[length(brks)] + xscl )   # make sure last bin goes outside range of data
    cnts <- hist( x$dist[x$dist<x$w.hi & x$dist>x$w.lo], plot=FALSE, breaks=brks, include.lowest=TRUE )
  }

  # Figure out scaling
  if( is.null( x$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- 0
    warning("g0 unspecified.  Assumed 1.")
  } else {
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
  }
  
  like <- match.fun( paste( x$like.form, ".like", sep=""))
  
  x.seq <- seq( x$w.lo, x$w.hi, length=200)
  
  if(!is.null(x$covars)){
    
    # compute column means of covariantes because need them later to scale bars
    covMeanMat <- col.m <- colMeans(x$covars)
    if("(Intercept)" %in% dimnames(x$covars)[[2]]){
      col.m <- col.m[-grep("(Intercept)",dimnames(x$covars)[[2]])]
    } 
    covMeans <- as.data.frame(matrix(col.m,1,length(col.m)))
    names(covMeans) <- names(col.m)
    covMeanMat <- matrix(covMeanMat, 1) # this has the intercept
    
    if(missing(newdata) || is.null(newdata)){
      # do something fancy here with factors and panels.
      newdata <- covMeans
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
               w.lo=x$w.lo, w.hi=x$w.hi, pointSurvey = FALSE )
    
    if(x$pointSurvey){
      f.at.x0 <- like( x$parameters, x0 - x$w.lo, series=x$series, expansions=x$expansions, 
                       w.lo=x$w.lo, w.hi=x$w.hi, pointSurvey = FALSE )
      scaler <- g.at.x0 / f.at.x0 # a length n vector 
      
      y <- y * scaler  # length(scalar) == nrow(y), so this works right
      y <- y * (x.seq - x$w.lo)
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
      plotBars <- TRUE
    } else {
      ybarhgts <- NULL
      yscl <- NULL
      plotBars <- FALSE
    }
  } else {
    if( !x$pointSurvey ){
      f.max <- F.maximize.g(x, covars = NULL) #like( x$parameters, x0 - x$w.lo, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, pointSurvey = x$pointSurvey )
      if(any(is.na(f.max) | (f.max <= 0))){
        #   can happen when parameters at the border of parameter space
        yscl <- 1.0
        warning("Y intercept missing or zero. One or more parameters likely at their boundaries. Caution.")
      } else {
        yscl <- g.at.x0 / f.max
      }
      if(length(yscl > 1)){yscl <- yscl[1]}
      y <- y * yscl
      ybarhgts <- cnts$density * yscl
      plotBars <- TRUE
    } else {
      ybarhgts <- NULL
      yscl <- NULL
      plotBars <- FALSE      
    }
  }
  
  y.finite <- y[ y < Inf ]
  y.lims <- c(0, max( g.at.x0, ybarhgts, y.finite, na.rm=TRUE ))
  
  if( include.zero ){
    x.limits <- c(0 , max(x.seq))
  } else {
    x.limits <- range(x.seq) 
  }
  
  if(plotBars){
    bar.mids <- barplot( ybarhgts, width=xscl, space=0, density=0, ylim=y.lims, 
                       xlim=x.limits, border="blue", ... )  
    xticks <- axTicks(1)
    axis( 1, at=xticks,  labels=xticks, line=.5 )
  } else {
    plot(1,1,type="n",ylim=y.lims, xlim=x.limits, xlab="",ylab="",bty="n")
  }
  title( xlab="Distance", ylab="Probability of detection" )
  if( !("main" %in% names(c(...))) ){
    # Put up a default title containing liklihood description
    if( x$expansions == 0 ){
      title(main=paste( x$like.form, ", ", x$expansions, " expansions", sep=""))
    } else {
      title(main=paste( x$like.form, ", ", x$series, " expansion, ", x$expansions, " expansions", sep=""))
    }
  }
  
  #   These 3 lines plot a polygon for the density function
  #x.poly <- c(0, x, x[length(x)] )
  #y.poly <- c(0, y, 0)
  #polygon( x.poly, y.poly, density=15, border="red", lwd=2 )
  
  #   This places a single line over the histogram
  if(is.matrix(y)){
    for(i in 1:ncol(y)){
      lines( x.seq, y[,i], col=i+1, lwd=2, lty = i )
    }
  }
  else{
    lines( x.seq, y, col="red", lwd=2 )
  }

  #assign("tmpxy",cbind(x.seq,y),envir = .GlobalEnv)
  
  #   These two add vertical lines at 0 and w
  lines( rep(x.seq[1], 2), c(0,y[1]), col="red", lwd=2 )
  lines( rep(x.seq[length(x.seq)], 2), c(0,y[length(x.seq)]), col="red", lwd=2 )
  
  #   print area under the curve
  area <- effectiveDistance(x)
  #area2 <- (x[3] - x[2]) * sum(y[-length(y)]+y[-1]) / 2   # use x[3] and x[2] because for hazard rate, x[1] is not evenly spaced with rest
  #print(c(area,area2))
  #text( max(x.seq), max(y.lims)-0.025*diff(y.lims), paste("ESW =", round(area,3)), adj=1)
  
  #   If model did not converge, print a message on the graph.
  if( x$convergence != 0 ){
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

    legend('topright', legend = Leg, lty = 1:nr, lwd = 2, col = 2:(nr+1), cex = 0.7)
  }
  
  #x$xscl.plot <- xscl   # gonna need this to plot something on the graph.
  x$yscl <- yscl   # this is g(x) / f(x).  Might want this later.
  
  invisible(x)
}