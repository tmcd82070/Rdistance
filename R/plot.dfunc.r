#' @name plot.dfunc
#' @aliases plot.dfunc
#' @title Plot a distance function.
#' @description Plot method for an estimated distance function. Estimated distance fnctions are of class 'dfunc'
#' @usage \method{plot}{dfunc}(x, include.zero = FALSE, nbins="Sturges", newdata = NULL, legend = TRUE, ...)
#' @param x An estimated distance function resulting from a call to \code{F.dfunc.estim}.
#' @param include.zero Boolean value specifying whether to include 0 in the plot.  A value of TRUE
#'   will include 0 on the left hand end of the x-axis regardless of the range of 
#'   distances.  A value of FALSE will plot only the range on input distanced.
#' @param nbins Internally, this function uses \code{hist} to compute histogram bars for the plot.  
#'   This argument is the \code{breaks} argument to \code{hist}.  This can be either a vector giving the 
#'   breakpoints between bars, a single number giving the suggested number of bars, a string naming an algorithm 
#'   to compute the number of bars, or a function to compute the number of bars.  See \code{help(hist)} 
#'   for all options.
#' @param newdata Matrix containing values of covariates to plot. Each row is a set of covariate values (i.e. each column contains all values of each covariate)
#' @param legend Boolean. If TRUE, a legend will be included on plot detailing covariate values plotted. 
#' @param \dots Other arguments to \code{barplot}, such as \code{cex}, \code{col}, \code{bty}, etc.
#'   The following plot parameters cannot be included in \dots: 
#'     \code{space}, \code{density}, \code{ylim}, \code{xlim}, and \code{border}.  In addition, 
#'   \code{main}, \code{ylab}, and \code{xlab} should not be used because the internal 
#'   values will overwrite whatever values are given.
#' @details A scaled histogram is plotted, and the estimated distance function is plotted over the top
#'   of it.  The form of the likelihood and any series expansions is printed in the main title.
#'   Convergence of the distance function is checked.  If the distance funtion did not converge, a 
#'   warning is printed over the top of the histogram.  If one or more parameter estimates are 
#'   at their limits (likely indicating non-covergence or poor fit), another warning is printed. 
#'   If the distance function did converge, the ESW is printed on the plot.
#' @return The input distance function is returned, with two additional components related to the plot
#'   that may be needed if additional lines or text is to added to the plot by the user.  These
#'   additional components are: 
#'     
#'   \item{xscl.plot}{Scaling factor for horizontal coordinates.  Due to the way \code{barplot} works, 
#'     the x-axis has been scaled.  The internal coordinates of the bars are 1, 2, \ldots, nbars.
#'     To plot something at a distance coordinate of x, x must be divided by this value.  For example, 
#'     to draw a vertical line at a value of 10 on the x-axis, the correct call is \code{abline(v=10/obj$xscl.plot)}.  }
#'   
#'   \item{yscl}{Scaling factor for vertical coordinates.  The histogram and distance function plotted by 
#'     this routine are scaled so that height of the distance function at \code{w.lo} is \code{g0}.  Usually, this means 
#'     the distance curve is scaled so that the y-intercept is 1, or that g(0) = 1.  To add a plot feature 
#'     at a real coordinate of y, y must be divided by this returned parameters.  For example, to draw 
#'     a horizontal line at y-axis coordinate of 1.0, issue \code{abline(h=1/obj$yscl)}.  }
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}}, \code{\link{print.dfunc}}, \code{\link{print.abund}}
#' @examples \dontrun{
#'   set.seed(87654)
#'   x <- rgamma(1000, 2, 2) 
#'   fit <- F.dfunc.estim(x, likelihood="Gamma")
#'   plot(fit)  
#'   }
#' @keywords models

plot.dfunc <- function( x, include.zero=FALSE, nbins="Sturges", newdata = NULL, legend = TRUE, ... ){
  #
  #   Plot method for distance functions.
  #
  #   input: x = object of class "dfunc"
  #   include.zero = whether or not to plot distance function at 0.
  
  #   changed the number of plotting points to 200 - jg
  if(!is.null(x$covars)){
    if(!is.null(newdata)){
      temp <- list()
      #newdata <- as.list(newdata)
      
      for(i in 1:nrow(newdata)){
        temp[[i]] <- unname(unlist(cbind(1, newdata)[i,]))
      }
      newdata <- temp
    }
    else{
      # Default covar values to plot
      temp <- NULL
      newdata <- list()
      for(i in 1:ncol(x$covars)){
        temp <- c(temp, mean(x$covars[,i]))
        if(!is.null(x$factor.names)){
          for(j in 1:length(x$factor.names)){
            if(startsWith(names(x$parameters)[i], x$factor.names[j])){
              temp[i] <- 0
            }
          }
        }
      }
      newdata[[1]] <- temp
    }
  }
  
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
  
  like <- match.fun( paste( x$like.form, ".like", sep=""))
  
  x.seq <- seq( x$w.lo, x$w.hi, length=200)
  if(!is.null(x$covars)){
    temp.covars <- list()
    for(i in 1:length(newdata)){
      temp.covars[[i]] <- matrix(nrow = length(x.seq), ncol = ncol(x$covars))
    }
  }
  if(is.list(newdata) & length(newdata) == 0)
    ncol = 1
  else
    ncol = length(newdata)
  y <- matrix(nrow = length(x.seq), ncol = ncol)
  
  if(!is.null(x$covars)){
    for(i in 1:length(newdata)){
      for(j in 1:length(x.seq)){
        temp.covars[[i]][j,] <- unname(newdata[[i]])
      }
      y[,i] <- like( x$parameters, x.seq - x$w.lo, covars = temp.covars[[i]], series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, point.transects = x$point.transects )
    }
  }
  else{
    y <- like( x$parameters, x.seq - x$w.lo, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, point.transects = x$point.transects )
  }
  
  if( include.zero & x$like.form == "hazrate" ){
    x.seq[1] <- x$w.lo
  }
  
  if( is.null( x$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- 0
    warning("g0 unspecified.  Assumed 1.")
  } else {
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
  }
  if(!is.null(x$covars)){
    for(i in 1:length(newdata)){
      f.max <- F.maximize.g(x, t(temp.covars[[i]][1,]))  #like( x$parameters, x0 - x$w.lo, covars = temp.covars[[i]], series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, point.transects = x$point.transects )
      if(any(is.na(f.max) | (f.max <= 0))){
        #   can happen when parameters at the border of parameter space
        yscl <- 1.0
        warning("Y intercept missing or zero. One or more parameters likely at their boundaries. Caution.")
      } else {
        yscl <- g.at.x0 / f.max
      }
      if(length(yscl > 1)){yscl <- yscl[1]}
      y[,i] <- y[,i] * yscl
    }
    temp <- NULL
    mean.covars <- matrix(nrow = length(x.seq), ncol = ncol(x$covars))
    for(i in 1:ncol(x$covars))
      temp <- c(temp, mean(x$covars[,i]))
    for(j in 1:length(x.seq)){
      mean.covars[j,] <- temp
    }
    f.max <- F.maximize.g(x, t(mean.covars[1,]))#like( x$parameters, x0 - x$w.lo, covars = mean.covars, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, point.transects = x$point.transects )
    yscl <- g.at.x0 / f.max
    if(length(yscl > 1)){yscl <- yscl[1]}
    ybarhgts <- cnts$density * yscl
  }
  else{
    f.max <- F.maximize.g(x, covars = NULL) #like( x$parameters, x0 - x$w.lo, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi, point.transects = x$point.transects )
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
  }
  
  y.finite <- y[ y < Inf ]
  y.lims <- c(0, max( g.at.x0, ybarhgts, y.finite, na.rm=TRUE ))
  
  if( include.zero ){
    x.limits <- c(0 , max(x.seq))
  } else {
    x.limits <- range(x.seq) 
  }
  
  
  bar.mids <- barplot( ybarhgts, width=xscl, space=0, density=0, ylim=y.lims, xlim=x.limits, border="blue", ... )   # real x coords are 1, 2, ..., nbars
  xticks <- axTicks(1)
  axis( 1, at=xticks,  labels=xticks, line=.5 )
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
  
  #   These two add vertical lines at 0 and w
  lines( rep(x.seq[1], 2), c(0,y[1]), col="red", lwd=2 )
  lines( rep(x.seq[length(x.seq)], 2), c(0,y[length(x.seq)]), col="red", lwd=2 )
  
  #   print area under the curve
  if(x$point.transects){
    area <- effective.radius(x)
  }
  else{
    area <- ESW(x)
  }
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
  } else if( is.na(area) | (area > x$w.hi) ){
    #   invalid scaling, g0 is wrong
    mess <- "Scaling failure"
    text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- paste("Check g0=", round(g.at.x0,2), "assumption")
    text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  }
  
  # Add legend to plot if covars are present
  if(legend & !is.null(x$covars)){
    legend.names <- vector(mode = "character", length = length(newdata))
    for(i in 1:length(newdata)){
      legend.factors <- vector(length = length(x$legend.names))
      for(j in 2:ncol(x$covars)){
        if(any(startsWith(colnames(x$covars)[j], as.character(x$factor.names)))){
          for(k in 1:length(x$factor.names)){
            if(startsWith(colnames(x$covars)[j], x$factor.names[k])){
              if(is.na(legend.factors[k])){
                legend.factors[k] <- paste0(newdata[[i]][j], ",")
              }
              else{
                legend.factors[k] <- paste0(legend.factors[k], signif(newdata[[i]][j],3), ",")
              }
            }
          }
        }
        
        else{
          legend.names[i] <- paste0(legend.names[i], " ", colnames(x$covars)[j], " = ", signif(newdata[[i]][j],3), ",")
        }
        
      }
      if(!is.null(x$factor.names)){
        for(k in 1:length(x$factor.names)){
          legend.names[i] <- paste0(legend.names[i], " ", x$factor.names[k], " = ", legend.factors[k])
        }
      }
      legend.names[i] <- substr(legend.names[i], 2, nchar(legend.names[i])-1)
    }
    legend('topright', legend = legend.names, lty = 1:ncol(y), lwd = 2, col = 2:(ncol(y)+1), cex = 0.7)
  }
  
  #x$xscl.plot <- xscl   # gonna need this to plot something on the graph.
  x$yscl <- yscl   # this is g(x) / f(x).  Might want this later.
  
  invisible(x)
}