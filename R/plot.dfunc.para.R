#' @title plot.dfunc.para - Plot parametric distance functions
#' 
#' @description
#' Plot method for parametric line and point transect distance functions. 
#' 
#' @inheritParams print.dfunc 
#'   
#' @param include.zero Boolean value specifying whether to include 0 on the x-axis 
#' of the plot.  A value of TRUE will include 0 on the left hand end of the x-axis
#' regardless of the range of distances.  A value of FALSE will plot only the
#' observation strip (\code{w.lo} to \code{w.hi}).
#'   
#' @param nbins Internally, this function uses \code{hist} to compute histogram
#'   bars for the plot. This argument is the \code{breaks} argument to
#'   \code{hist}.  This can be either a vector giving the breakpoints between
#'   bars, the suggested number of bars (a single number), a string naming
#'   an algorithm to compute the number of bars, or a function to compute the
#'   number of bars.  See \code{\link{hist}} for all options.
#'   
#' @param newdata Data frame (similar to \code{newdata} parameter 
#' of \code{\link{lm}}) containing new values for covariates in 
#' the distance function.
#' One distance function is computed and plotted for each row in the data frame. 
#' If \code{newdata} is NULL, a single distance function is plotted 
#' for mean values of all numeric covariates and mode values for all 
#' factor covariates. 
#'   
#' @param legend Logical scalar for whether to include a legend. 
#'   If TRUE, a legend will be included on the plot detailing
#'   the covariate values used to generate the plot.
#'   
#' @param plotBars Logical scalar for whether to plot the histogram 
#' of distances behind the distance function.  If FALSE, no histogram 
#' is plotted, only the distance function line(s).
#'  
#' @param circles Logical scalar indicating whether to plot 
#' small circles on the distance function(s) at the locations 
#' of observed distances. Note that one circle plots on each distance 
#' function for every observed distance even though covariates 
#' associated with the distances may not match the function. 
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
#' If \code{density} > 0, bars are shaded with colors and angles from 
#' parameters \code{col} and \code{angle}.
#' 
#' @param angle When \code{density} > 0, the slope of bar shading lines, 
#' given as an angle in degrees (counter-clockwise), repeated as necessary
#' to exceed the number of bars.
#' 
#' @param col A vector of bar fill colors or line colors when bars are 
#' drawn and \code{density != 0}, repeated as necessary to exceed the number
#' of bars. Also used for the bar borders when
#' \code{border = TRUE}.
#' 
#' @param circles Logical scalar requesting the location of detection distances
#' be plotted. If TRUE, open circles are plotted at predicted distance 
#' function heights associated with all detection distances. For computational
#' simplicity, all distances are plotted for EVERY covariate class even though
#' observed distances belong to only one covariate class. If FALSE, circles 
#' are not plotted. 
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
#'  uses \code{graphics::barplot} to draw the 
#'  plotting region and bars. When bars are not plotted,
#'  this routine sets up the plot with \code{graphics::plot}.
#'  \dots can be any argument to \code{barplot} or \code{plot} EXCEPT  
#'  \code{width}, \code{ylim}, \code{xlim}, 
#'  \code{density}, \code{angle}, and \code{space}. For example, 
#'  set the main title with \code{main = "Main Title"}.
#' 
#' @inherit plot.dfunc return
#' 
#' @seealso \code{\link{plot.dfunc}}
#' 
#' @examples
#' 
#' # Example data
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' x <- units::set_units(x, "ft")
#' Df <- data.frame(transectID = "A"
#'                , distance = x
#'                 ) |> 
#'   dplyr::nest_by( transectID
#'                , .key = "detections") |> 
#'   dplyr::mutate(length = units::set_units(1,"mi"))
#' attr(Df, "detectionColumn") <- "detections"
#' attr(Df, "obsType") <- "single"
#' attr(Df, "transType") <- "line"
#' attr(Df, "effortColumn") <- "length"
#' is.RdistDf(Df) # TRUE
#' 
#' # Estimation
#' dfunc <- dfuncEstim(Df
#'                   , formula = distance~1
#'                   , likelihood="halfnorm")
#' plot(dfunc)
#' plot(dfunc, nbins=25)
#'
#' @importFrom stats predict
#' @importFrom graphics hist par barplot axTicks axis lines matpoints text
#' @importFrom grDevices rainbow
plot.dfunc.para <- function( x, 
                        include.zero=FALSE, 
                        nbins="Sturges", 
                        newdata = NULL, 
                        legend = TRUE, 
                        vertLines = TRUE,
                        plotBars = TRUE,
                        circles = FALSE,
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
  
  d <- Rdistance::distances(x)
  whi <- x$w.hi
  wlo <- x$w.lo
  
  cnts <- graphics::hist( d
                , plot = FALSE
                , breaks = nbins
                , include.lowest = TRUE
                , warn.unused = FALSE)
  
  # hist should return breaks with units attached, but it does not
  cnts$breaks <- units::as_units(cnts$breaks, x$outputUnits)
  cnts$mids <- units::as_units(cnts$mids, x$outputUnits)
  xscl <- diff(cnts$breaks) 
  x.seq <- seq( x$w.lo, x$w.hi, length=getOption("Rdistance_intEvalPts") )
  
  
  # Fixup new data if missing ----
  if( is.null(newdata) ){
    
    # Function to return mode (=most frequent values) of a FACTOR.
    # only needed in this case.
    getmode <- function(v) {
      uniqv <- table(v)
      modeVal <- names(uniqv)[which.max(uniqv)] 
      if( is.factor(v) ){
        modeVal <- factor(modeVal, levels = levels(v))
      } 
      if( is.character(v) ){
        modeVal <- factor(modeVal, levels = names(uniqv))
      }
      modeVal
    }
    
    # Note: x$mf is the model frame. It has only non-missing values between w.lo and w.hi
    # x$mf[,-1] has covariates in un-expanded-for-indicator variables format 
    
    covNames <- labels(terms(x$mf)) # Intercept not included here
    newdata <- matrix(NA, nrow = 1, ncol = length(covNames))
    colnames(newdata) <- covNames
    newdata <- data.frame(newdata)
    
    # Watch filtering of in-out strip observations here
    
    # origDist <- Rdistance::distances(x) 
    # inStrip <- (x$w.lo <= d) & (d <= x$w.hi)
    factor.names <- attr(terms(x$mf), "dataClasses")
    factor.names <- names(factor.names)[ factor.names %in% c("factor","character") ]
    for( nm in covNames ) {
      if( nm %in% factor.names ) {
        newdata[,nm] <- getmode(x$mf[, nm]) # Use mode to predict
      } else {
        newdata[,nm] <- mean(x$mf[, nm]) # Use mean
      }
    }
  }

  # Add discrete likelihood points ----
  # add points just left and just right of breaks in discontinuous functions
  x.seq <- switch(x$likelihood
                  , "oneStep" = insertOneStepBreaks(obj = x
                                                  , newData = newdata
                                                  , xseq = x.seq)
                  , x.seq
  )
  
    
  # Predict distance functions ----
  # after here, y is a matrix, columns are distance functions.
  y <- stats::predict(object = x
                      , newdata = newdata
                      , distances = x.seq
                      , type = "dfuncs"
  )

  # Compute scaling factors ----
  if( Rdistance::is.points(x) ){
    y <- y * units::set_units(x.seq - x$w.lo, NULL)
    y <- t( t(y) / (colSums(y, na.rm = TRUE) * units::set_units(x.seq[2] - x.seq[1], NULL))) # now y integrates to 1.0
    # don't need to modify ybarhgts because cnts$density integrates to 1.0 already
    ybarhgts <- cnts$density
    y.finite <- y[ y < Inf ]
     # scaler <- (units::set_units(scaler, NULL) ^ 2) / 2 # = integral of y = sum(y) * (x.seq[2] - x.seq[1])
     # scaler <- units::drop_units(x.seq[2]-x.seq[1]) * colSums(y[-nrow(y),,drop = FALSE]+y[-1,,drop = FALSE]) / 2
     # ybarhgts <- ybarhgts * scaler
    y.lims <- c(0, max( ybarhgts, y.finite, na.rm=TRUE ))
  } else {
    scaler <- Rdistance::effectiveDistance(object = x
                                           , newdata = newdata)
    # Note: scaler is correct even when g.x.scl != 1. Hence, no need to apply 
    # another scaler. i.e., this works when g.x.scl < 1
    ybarhgts <-  cnts$density * units::set_units(mean(scaler), NULL) # now ybarhgts integrates to ESW 
    y.finite <- y[ y < Inf ]
    y.lims <- c(0, max( x$g.x.scl, ybarhgts, y.finite, na.rm=TRUE ))
  }

  # I DON'T THINK THIS IS NEEDED, BUT MAYBE  
  # if( include.zero & x$like.form == "hazrate" ){
  #   x.seq[1] <- x$w.lo
  # }
  
  
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
    if( Rdistance::is.points(x)){
      ylab <- "Observation density"
    } else {
      ylab <- "Probability of detection"
    }
  }
  
  # Main plot ----
  if(plotBars){
    if(x$w.lo != zero){
      ybarhgts <- c(NA,ybarhgts)
      xscl <- c(xscl[1], xscl)
      # the following is because barplot draws the border
      # of the NA box when line density >= 0.  Makes no sense, but there it is.
      # This produces a line to 0 when w.lo > 0
      if( any(density > 0) ){
        warning("Line 'density' of bars cannot be positive when w.lo > 0. Set to 0 or negative.")
        density[ density > 0 ] <- -1
      }
      if( any(density == 0) ){
        # bars are supposed to be empty when density = 0, so reset to fill with background
        col[density == 0] = graphics::par()$bg
        density[density == 0] = -1
      }
    }
    bar.mids <- graphics::barplot( ybarhgts, 
                         width = units::set_units(xscl, NULL), 
                         ylim = y.lims, 
                         xlim = units::set_units(x.limits, NULL),
                         space = 0, 
                         density = density,
                         angle = angle,
                         col = col,
                         border = border,
                         xlab = xlab,
                         ylab = ylab,
                         ... )  
    xticks <- graphics::axTicks(1)
    graphics::axis( 1, at=xticks,  labels=xticks, line=.5, ... )
  } else {
    plot(1,1,type="n",
         ylim = y.lims, 
         xlim = x.limits, 
         xlab = xlab,
         ylab = ylab,
         bty = "n",
         ...)
  }
  
  #   Work out the colors, line types, and line widths ----
  nFunctions <- ncol(y)
  if( is.null(col.dfunc) ){
    # rainbow(1) is red, the default for one line
    col.dfunc <- grDevices::rainbow(nFunctions)
  } else if(length(col.dfunc) < nFunctions){
    col.dfunc <- rep(col.dfunc,ceiling(nFunctions/length(col.dfunc)))[1:nFunctions]
  }
  if( is.null(lty.dfunc) ){
    lty.dfunc <- rep(1,nFunctions)
  } else if(length(lty.dfunc) < nFunctions){
    lty.dfunc <- rep(lty.dfunc,ceiling(nFunctions/length(lty.dfunc)))[1:nFunctions]
  }
  if( is.null(lwd.dfunc) ){
    lwd.dfunc <- rep(2,nFunctions)
  } else if(length(lwd.dfunc) < nFunctions){
    lwd.dfunc <- rep(lwd.dfunc,ceiling(nFunctions/length(lwd.dfunc)))[1:nFunctions]
  }

  # Plot circles if called for ----
  if(circles){
    d <- Rdistance::distances(x)
    g <- apply(y, 2, FUN = function(y, x.seq, d){
      approx(x.seq, y, xout = d)$y
    }
    , x.seq = x.seq
    , d = d )
    graphics::matpoints(d, g, pch = 1, lty = 1, col = 1, cex = 1)
  }
  
  # Draw distance functions ----
  for(i in 1:nFunctions){
    graphics::lines( x.seq, y[,i]
           , col=col.dfunc[i]
           , lwd=lwd.dfunc[i]
           , lty = lty.dfunc[i]
           , xpd = TRUE
    )
  }
  
  #   Add vertical lines at 0 and w if called for ----
  if(vertLines){
    graphics::lines( rep(x.seq[1], 2), c(0,y[1,1]), col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1])
    graphics::lines( rep(x.seq[length(x.seq)], 2), c(0,y[length(x.seq), 1]), 
           col=col.dfunc[1], lwd=lwd.dfunc[1],
           lty=lty.dfunc[1] )
  }
  

  # Check convergence ----
  if( (x$likelihood != "smu") && x$convergence != 0 ){
    if( x$convergence == -1 ){
      mess <- "Solution failure"
    } else {
      mess <- "Convergence failure"
    }
    graphics::text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    graphics::text( mean(x.seq), mean(y.lims), paste("\n\n\n", x$fit$message, sep=""), cex=1, adj=.5, col="black")
  }
  
  # Check for Probabilities > 1 ----
  if( !is.points(x) && any(y > 1) ){
    # some g(x) > 1, should'nt happen for line transects
    mess <- "Probabilities > 1"
    graphics::text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- "g(x) > 1 should not happen"
    graphics::text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
    mess <- paste0("Check g(", format(x$x.scl), ")= ", round(x$g.x.scl,2), " assumption")
    graphics::text( mean(x.seq), mean(y.lims), paste("\n\n\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  } 
  
  # Check missing or <0 y values ----
  if( any(is.na(y)) | any(y < 0) ){
    #   invalid scaling, g0 is wrong
    mess <- "Probabilities < 0"
    graphics::text( mean(x.seq), mean(y.lims), mess, cex=3, adj=.5, col="red")
    mess <- paste("One or more parameters likely at boundary")
    graphics::text( mean(x.seq), mean(y.lims), paste("\n\n\n", mess,sep=""), cex=1, adj=.5, col="black")
  }
  
  # Legend ----
  if(legend & (x$nCovars != 0) ){
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
    
    graphics::legend('topright', legend = Leg, lty = lty.dfunc, lwd = lwd.dfunc, 
           col = col.dfunc, cex = 0.7)
  }
  
  # Clean up ----
  # x$yscl <- scaler   # this is g(x) / f(x) = ESW if lines. One for each row in newdata.  Might want this later.
  x$barHeights <- ybarhgts  # scaled to mean scaler.
  x$barWidths <- xscl
  x$predCovValues <- newdata
  
  invisible(x)
  
}