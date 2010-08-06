\name{F.nLL}
\alias{F.nLL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F.nLL(a, dist, like, w.lo = 0, w.hi = max(dist), series, expansions = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
  \item{like}{
%%     ~~Describe \code{like} here~~
}
  \item{w.lo}{
%%     ~~Describe \code{w.lo} here~~
}
  \item{w.hi}{
%%     ~~Describe \code{w.hi} here~~
}
  \item{series}{
%%     ~~Describe \code{series} here~~
}
  \item{expansions}{
%%     ~~Describe \code{expansions} here~~
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
function(a, dist, like, w.lo=0, w.hi=max(dist), series, expansions=0){

    f.loglike <- match.fun(paste( like, ".like", sep=""))

    LL <- f.loglike( a, dist, w.lo=w.lo, w.hi=w.hi, series, expansions )

    LL[ !is.na(LL) & (LL <= 0) ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(LL), na.rm=T)  # Note that distances > w in LL are set to NA
    nLL
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
