\name{plot.dfunc}
\alias{plot.dfunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.dfunc(obj, include.zero = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{include.zero}{
%%     ~~Describe \code{include.zero} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function( obj, include.zero=FALSE, ... ){
#
#   Plot method for distance functions.
#
#   input: obj = object of class "dfunc"
#   include.zero = whether or not to plot distance function at 0. 

#   changed the number of plotting points to 200 - jg
cnts <- hist( obj$dist, plot=F)
xscl <- cnts$mid[2] - cnts$mid[1]

like <- match.fun( paste( obj$like.form, ".like", sep=""))

x <- seq( obj$w.lo, obj$w.hi, length=200)

y <- like( obj$parameters, x - obj$w.lo, series=obj$series, expansions=obj$expansions, w.lo=obj$w.lo, w.hi=obj$w.hi )

if( include.zero & obj$like.form == "hazrate" ){
    x[1] <- obj$w.lo
  }

if( is.null( obj$g.x.scl ) ){
    #   Assume g0 = 1
    g.at.x0 <- 1
    x0 <- 0
    warning("g0 unspecified.  Assumed 1.")
  } else {
    g.at.x0 <- obj$g.x.scl
    x0 <- obj$x.scl
  }
f.at.x0 <- like( obj$parameters, x0 - obj$w.lo, series=obj$series, expansions=obj$expansions, w.lo=obj$w.lo, w.hi=obj$w.hi )
if(is.na(f.at.x0) | (f.at.x0 <= 0)){
    #   can happen when parameters at the border of parameter space
    yscl <- 1.0
    warning("Y intercept missing or zero. One or more parameters likely at their boundaries. Caution.")
  } else {
    yscl <- g.at.x0 / f.at.x0
  }
ybarhgts <- cnts$density * yscl
y <- y * yscl

y.lims <- c(0, max( g.at.x0, ybarhgts ))
    
if( include.zero ){
    x.limits <- c(0 , max(x)/xscl)
  } else {
    x.limits <- range(x) / xscl
  }
    
bar.mids <- barplot( ybarhgts, space=0, density=0, ylim=y.lims, xlim=x.limits, border="blue", ... )   # real x coords are 1, 2, ..., nbars
xticks <- axTicks(1)
axis( 1, at=xticks,  labels=xticks * xscl, line=.5 )
title( xlab="Distance", ylab="Probability of detection" )
if( obj$expansions == 0 ){
    title(main=paste( obj$like.form, ", ", obj$expansions, " expansions", sep=""))
  } else {
    title(main=paste( obj$like.form, ", ", obj$series, " expansion, ", obj$expansions, " expansions", sep=""))
  }

#   These 3 lines plot a polygon for the density function
#x.poly <- c(0, x/xscl, (x/xscl)[length(x)] )
#y.poly <- c(0, y, 0)
#polygon( x.poly, y.poly, density=15, border="red", lwd=2 )

#   This places a single line over the histogram
lines( x/xscl, y, col="red", lwd=2 )

#   These two add vertical lines at 0 and w
lines( rep(x[1]/xscl, 2), c(0,y[1]), col="red", lwd=2 )
lines( rep(x[length(x)]/xscl, 2), c(0,y[length(x)]), col="red", lwd=2 )

#   print area under the curve
area <- ESW( obj )
#area2 <- (x[3] - x[2]) * sum(y[-length(y)]+y[-1]) / 2   # use x[3] and x[2] because for hazard rate, x[1] is not evenly spaced with rest
#print(c(area,area2))
text( max(x)/xscl, max(y.lims)-0.025*diff(y.lims), paste("ESW =", round(area,3)), adj=1)

#   If model did not converge, print a message on the graph.
if( obj$fit$convergence != 0 ){
    if( obj$fit$convergence == -1 ){
        mess <- "Solution failure"
    } else {
        mess <- "Convergence failure"
    }
    text( mean(x)/xscl, mean(y.lims), mess, cex=3, adj=.5, col="red")
    text( mean(x)/xscl, mean(y.lims), paste("\n\n\n", obj$fit$message, sep=""), cex=1, adj=.5, col="black")
  }

obj$xscl.plot <- xscl   # gonna need this to plot something on the graph.
obj$yscl <- yscl   # this is g(x) / f(x).  Might want this later.

invisible(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
