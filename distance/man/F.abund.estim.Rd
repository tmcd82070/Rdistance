\name{F.abund.estim}
\alias{F.abund.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F.abund.estim(dfunc, avg.group.size = 1, group.sizes, area = 1, tot.trans.len = 1, n = length(dfunc$dist))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfunc}{
%%     ~~Describe \code{dfunc} here~~
}
  \item{avg.group.size}{
%%     ~~Describe \code{avg.group.size} here~~
}
  \item{group.sizes}{
%%     ~~Describe \code{group.sizes} here~~
}
  \item{area}{
%%     ~~Describe \code{area} here~~
}
  \item{tot.trans.len}{
%%     ~~Describe \code{tot.trans.len} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function( dfunc, avg.group.size=1, group.sizes, area=1, tot.trans.len=1, n=length(dfunc$dist) ){
#
#   Estimate abundance using an estimated distance function
#
#   Inputs:
#   dfunc = an estimated distance function of class 'dfunc'
#   avg.group.size = average group size, from some other source.
#   
#   group.size = vector of group sizes associated with each sighted object
#   area = total area of the study area.  To estimate density, set area = 1.
#   tot.trans.len = total length of transects surveyed.  Units are square root of units of area
#   n = number of groups seen, Normally, this is length(dist), but left as a parameter 
#       so that situations where you want to apply a distance function to another set of surveys 
#       can be used. 

#   There are two situations:  you have avg.group.size and n; you have group.sizes for every sighted group.

if( missing(group.sizes) ){
    #   use avg group size times n
    n.tot <- avg.group.size * n
  } else if( length(group.sizes) == n ){
    n.tot <- sum(group.sizes)
  } else {
    stop("Either avg.group.size or group.sizes vector must be specified")
  }

esw <- ESW( dfunc )

n.hat <- n.tot * area / (2*esw*tot.trans.len)

ans <- dfunc
ans$n.hat <- n.hat
ans$se.n.hat <- NA   #  Need to get this from the book.  Make a summary method that uses bootstrapping. 

class(ans) <- c( "abund", class(dfunc))

ans

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
