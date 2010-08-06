\name{hazrate.like}
\alias{hazrate.like}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hazrate.like(a, dist, w.lo = 0, w.hi = max(dist), series = "cosine", expansions = 0, scale = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
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
  \item{scale}{
%%     ~~Describe \code{scale} here~~
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
function(a, dist, w.lo=0, w.hi=max(dist), series="cosine", expansions=0, scale=TRUE){
#
#   Compute hazard rate likelihood
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions.
#       a must be at least length = 2
#   dist = input observed distance data
#   w = right truncation value, same units as dist
#   series = character values specifying type of expansion.  Currently only
#       "cosine", "simple", and "hermite" work. Default is no series expansions. Default is "cosine"
#   expansions = number of expansion terms.  This controls whether series
#       expansion terms are fitted. Default = 0 does not fit any terms.
#       If >0, fit the number of expansion terms specified of the type
#       specified by series.  Max terms depends on series.
#   scale = logical, whether or not to scale the likelihood values so 
#       that the underlying density integrates to 1. This parameter is used 
#       to stop the recursion. 
#
#   Output:
#   A vector same length as dist, containing likelihood values, scaled appropriately.
#   Likelihood for all distances >w are set to NA
#	

    dist[ (dist < w.lo) | (dist > w.hi) ] <- NA
	sigma=a[1]
	beta=a[2]
	key = 1 - exp(-(dist/sigma)^(-beta))
    dfunc <- key
    w <- w.hi - w.lo


    if(expansions > 0){

        nexp <- min(expansions,length(a)-2)  # should be equal. If not, fire warning next
        
        if( length(a) != (expansions+2) ) {
            warning("Wrong number of parameters in expansion. Should be (expansions+2). Higher terms ignored.")
        }

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/sigma
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + (exp.term \%*\% a[3:(nexp+2)]))


    } else if(length(a) > 2){
        warning("Wrong number of parameters in hazrate. Only 2 needed if no expansions. Higher terms ignored.")
    }

    if( scale ){
        dfunc = dfunc / integration.constant(hazrate.like, w.lo=w.lo, w.hi=w.hi, a=a, series=series,expansions=expansions)
    }
    
    c(dfunc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
