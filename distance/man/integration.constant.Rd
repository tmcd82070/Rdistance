\name{integration.constant}
\alias{integration.constant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
integration.constant(density, w.lo, w.hi, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{w.lo}{
%%     ~~Describe \code{w.lo} here~~
}
  \item{w.hi}{
%%     ~~Describe \code{w.hi} here~~
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
function( density, w.lo, w.hi, ... ){
#
#   Return the scalar so that integral from 0 to w of underlying density
#   is 1.0
#
#   Input:
#   density = a function to compute integration constant for.
#       this function must be capable of evaluating values from 0 to w
#   w = upper limit of integral.
#   ... = additional arguments to density.  These vary by density function,
#       but generally are parameter values, series, expansion terms, etc.
#
#   Output:
#   a divisor scalar such that density / scalar integrates to 1.0. i.e.,
#   this output scalar is the integral of unscaled density from 0 to w.
#

density = match.fun(density)

seqx = seq(w.lo, w.hi, length=200)
seqy = density( dist=seqx, scale=FALSE, w.lo=w.lo, w.hi=w.hi,...)

#   Trapazoid rule
scaler= (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2

scaler

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
