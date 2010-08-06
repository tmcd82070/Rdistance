\name{F.dfunc.estim}
\alias{F.dfunc.estim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F.dfunc.estim(dist, likelihood = "halfnorm", w.lo = 0, w.hi = max(dist), expansions = 0, series = "cosine")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
  \item{likelihood}{
%%     ~~Describe \code{likelihood} here~~
}
  \item{w.lo}{
%%     ~~Describe \code{w.lo} here~~
}
  \item{w.hi}{
%%     ~~Describe \code{w.hi} here~~
}
  \item{expansions}{
%%     ~~Describe \code{expansions} here~~
}
  \item{series}{
%%     ~~Describe \code{series} here~~
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
function(dist, likelihood="halfnorm", w.lo=0, w.hi=max(dist), expansions=0, series="cosine")
{
# Remember to describe naming convention in help files.  Naming convention is <form>.nLL() returns the negative log liklihood value (i.e., a scalar). 
# <form>.like() returns the likelihood evaluated at every point (i.e., a vector)
# likelihood is a string naming <form>


#Expansion terms work - jg - 5/4/10

call <- match.call()

#   Set starting values and limits for parameters
strt.lims <- F.start.limits( likelihood, expansions, w.lo, w.hi, dist )

#   Minimize the negative sum of the loglikelihood.
fit<- nlminb(strt.lims$start, F.nLL,  lower=strt.lims$lowlimit,
      upper=strt.lims$uplimit, control=list(trace=0,iter.max=1000), 
      dist=dist, like=likelihood, w.lo=w.lo, w.hi=w.hi, expansions=expansions, series=series) 

#   Assign names to parameters here
names(fit$par) <- strt.lims$names

#   Check for convergence and parameters at the boundaries
fuzz <- 1e-6
low.bound <- any( fit$par <= (strt.lims$lowlimit + fuzz) )
high.bound <- any( fit$par >= (strt.lims$uplimit - fuzz) )

gx <- list( x.scl=0, g.x.scl=1 )
if( fit$convergence != 0 ){
    warning(fit$message)
  } else if( low.bound | high.bound ){
    fit$convergence <- -1
    fit$message <- "One or more parameters at its boundary."
    warning(fit$message)
  } else {
#   Set g(0), or g(x).
    gx <- F.gx.estim( fit, likelihood, w.lo )  # returns components for x and g(x)
  }


ans <- list(    parameters=fit$par,
                loglik = fit$objective,
                convergence = fit$convergence, 
                like.form = likelihood,
                x.scl = gx$x.scl,
                g.x.scl = gx$g.x.scl,
                w.lo = w.lo,
                w.hi = w.hi,
                dist = dist,
                expansions=expansions,
                series = series,
                call = call,                
                fit=fit )               
    
class( ans ) <- "dfunc" 

    
ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
