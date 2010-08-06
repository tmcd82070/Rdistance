\name{print.dfunc}
\alias{print.dfunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.dfunc(obj, digits = max(3, getOption("digits") - 3), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function( obj, digits = max(3, getOption("digits") - 3), ... ){
#
#   Print a distance function
#

    cat("Call: ", deparse(obj$call), "\n\n", sep = "")
    if (length(coef(obj))) {
        cat("Coefficients:\n")
        print.default(format(coef(obj), digits = digits), print.gap = 2,
            quote = FALSE)
    }
    else cat("No coefficients\n")
    
    cat("\n")
    
    if( obj$convergence == 0 ){
        mess <- "Success"
    } else {
        mess <- "FAILURE"
    }
    cat(paste("Convergence: ", mess, " (Exit code=", obj$convergence, ", ", obj$fit$message, ")", "\n", sep="")) 
    
    
    if( obj$expansions==0 ){
        mess <- ""
    } else {
        mess <- paste( "with", obj$expansions, "expansion(s) of", casefold( obj$series, upper=TRUE ), "series")
    }
    cat(paste("Function:", casefold(obj$like.form, upper=TRUE), mess, "\n") )
    
    cat(paste("Strip:", obj$w.lo, "to", obj$w.hi, "\n"))
    cat(paste("Effective strip width:", ESW(obj), "\n"))
    cat(paste("Scaling: g(", obj$x.scl, ") = ", obj$g.x.scl, "\n", sep=""))
    cat(paste("Log likelihood:", round(obj$loglik, digits), "\n")) 
    cat(paste("AIC:", round(AIC(obj), digits), "\n"))
    
    
    cat("\n")
    invisible(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
