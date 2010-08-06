\name{F.gx.estim}
\alias{F.gx.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F.gx.estim(fit, likelihood, w.lo)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{likelihood}{
%%     ~~Describe \code{likelihood} here~~
}
  \item{w.lo}{
%%     ~~Describe \code{w.lo} here~~
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
function( fit, likelihood, w.lo ){
#
#   Estimate g0 or gx for the distance function in fit.
#

if( likelihood == "gamma" ){
    r <- fit$par[1]
    lam <- fit$par[2]
    b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
    x.scl <- lam * b * (r - 1)
    g.x <- 1    # or, eventually, h
  } else {
    #   The default for all likelihoods not specified above
    x.scl <- w.lo
    g.x <- 1
  }

list( x.scl = x.scl, g.x.scl = g.x )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
