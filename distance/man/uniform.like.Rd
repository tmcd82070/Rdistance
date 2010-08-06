\name{uniform.like}
\alias{uniform.like}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
uniform.like(a, dist, w.lo = 0, w.hi = max(dist), series = "cosine", expansions = 0, scale = TRUE)
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
function(a, dist, w.lo=0, w.hi=max(dist), series="cosine", expansions= 0, scale=TRUE){
#
#   Compute the uniform likelihood, scaled appropriately, for all distance values in dist.
#
#   The likelihood is actually a "heavy-side" function of the form, 
#
#       f(x; a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp( -b*(x-a) ) / (1 + exp( -b*(x-a) ))
#
#   which you can see is basically a logistic function.  Parameter a is the location of the 
#   "discontinuity" (=inversef(.5) = a) and b is the "knee" or sharpness of the knee at a.
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions. If 
#       no expansion terms called for (ie., straight logistic) a is 
#       length 2, where a[1] is upper end of support (i.e., density is approximately U[0,a]), 
#       and a[2] is sharpness of the decline (or knee) at a[1]
#       If expansions are called for a[3:length(a)] are coefficients of the 
#       expansion.
#   dist = input observed distance data
#   w = right truncation value, same units as dist
#   series = character values specifying type of expansion.  Currently only
#       "cosine" and "simple" work. Default is no series expansions.
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

    #   A couple internal functions first.
    #   This is the heavy-side function.  Basically, a steep logistic. f is just heavi flipped over
    heavi <- function(x,k){ 1 / (1 + exp( -k*x ))}
    f <- function(beta, x){ 1 - heavi(x-beta[1],beta[2]) }

    
    dist[ (dist < w.lo) | (dist > w.hi) ] = NA

	key <- f(a,dist)
    dfunc <- key
    w <- w.hi - w.lo
#    cat(paste( "w.lo=", w.lo, "w.hi=", w.hi, "\n"))

    # If there are expansion terms
    if(expansions > 0){

        nexp <- min(expansions,length(a)-2)  # should be equal. If not, fire warning next
        
        if( length(a) != (expansions+2) ) {
            warning("Wrong number of parameters in expansion. Should be (expansions+2). High terms ignored.")
        }

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/ (a[1]/sqrt(12))  # denom is approx std of U[0,a[1]]
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + (exp.term \%*\% a[3:(nexp+2)]))
    }

    if( scale ){
            dfunc = dfunc / integration.constant( uniform.like, w.lo=w.lo,w.hi=w.hi, a=a,series=series,expansions=expansions )   # scales density so integrate from w.lo to w.hi is 1.0
    }

#   df2 <- dfunc[ order(dist) ]
#   d2 <- dist[ order(dist) ]
#   cat(paste("integral=", sum( diff(d2) * (df2[-1] + df2[-length(df2)]) ) / 2, "\n" ))
#   readline("Enter:")

    c(dfunc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
