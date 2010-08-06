\name{F.start.limits}
\alias{F.start.limits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F.start.limits(like, expan, w.lo, w.hi, dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{like}{
%%     ~~Describe \code{like} here~~
}
  \item{expan}{
%%     ~~Describe \code{expan} here~~
}
  \item{w.lo}{
%%     ~~Describe \code{w.lo} here~~
}
  \item{w.hi}{
%%     ~~Describe \code{w.hi} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
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
function( like, expan, w.lo, w.hi, dist ){
#
#   Establish starting value for parameters, and limits passed to the optimizer
#

#   Number of parameters
np <- expan + 1 + 1*(like \%in\% c("hazrate","uniform"))

w <- w.hi - w.lo

#   No starting values given
if( like == "hazrate" ){
    start <- c(.5*w, 1,rep(0, np - 2))
    low   <- c(0, .01, rep(-Inf, np - 2 ))
    high  <- c(Inf, Inf, rep( Inf, np - 2 ))
    nms <- c("Sigma", "Beta")
    if(expan > 0) nms <- c(nms, paste( "a", 1:(np-2), sep=""))

  } else if( like == "halfnorm" ){
    start <- c(0.5*w, rep(0, np - 1))
    low   <- c(0, rep(-Inf, np - 1 ))
    high  <- c(Inf, rep( Inf, np - 1 ))
    nms <- c("Sigma")
    if(expan > 0) nms <- c(nms, paste( "a", 1:(np-1), sep=""))

  } else if( like == "uniform" ){
    start <- c(.1*w, 1, rep(0, np - 2))
    low   <- c(1e-6, 0, rep(-Inf, np - 2 ))
    high  <- c(w, Inf, rep( Inf, np - 2 ))
    nms <- c("Threshold", "Knee")
    if(expan > 0) nms <- c(nms, paste( "a", 1:(np-2), sep=""))

  } else if( like == "negexp" ){
    start <- c(1, rep(0, np - 1))
    low   <- c(0, rep(-Inf, np - 1 ))
    high  <- c(Inf, rep( Inf, np - 1 ))
    nms <- c("Beta")
    if(expan > 0) nms <- c(nms, paste( "a", 1:(np-1), sep=""))
    

  } else if( like == "gamma" ){
    s <- log( mean(dist, na.rm=TRUE) ) - mean( log(dist), na.rm=TRUE )
    s2 <- (s-3)^2 + 24*s
    if( s2 < 0 ) s2 <- 0
    r <- (3 - s + sqrt( s2 )) / (12*s)
    if( r <= 1 ) r <- 1.01
    b <- ( (r-1) / exp(1) )^(r-1) / gamma(r)
    lam <- mean(dist) / (r * b)
    
    start <- c(r, lam)
    low   <- c(1.0001, 0)
    high  <- c(Inf, Inf)
    nms <- c("Shape", "Scale")
    
    
  }

names(start) <- nms
names(low) <- nms
names(high) <- nms

list( start=start, lowlimit=low, uplimit=high, names=nms )

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
