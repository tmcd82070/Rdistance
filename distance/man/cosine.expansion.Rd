\name{cosine.expansion}
\alias{cosine.expansion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cosine.expansion(x, expansions)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(x, expansions){
# Calculates cosine expansion for detection function.
# Input:
#       x = distances / w
#       expansions = number of expansion terms (1 - 5)
#
# Output:
#       expansion = a matrix with columns
#                   expansion[,1] = expansion for the 1st term,
#                   expansion[,2] = expansion for the 2nd, and so on.  
# changed 1st coeff to 2 - even function - jg
   
    if (expansions > 5){
        warning("Too many Cosine polynomial expansion terms. Only 5 used.")
        expansions = 5
    }
    
    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
    
    expansion = matrix(nrow=length(x), ncol=expansions)

    expansion[,1] = cos(2*pi*x)
    
    # I realize I could do this in a for loop, but I think this is faster.    
    if(expansions >= 2){
        expansion[,2] = cos(3*pi*x)
    }
    if(expansions >= 3){
        expansion[,3] = cos(4*pi*x)
    }
    if(expansions >= 4){
        expansion[,4]=  cos(5*pi*x)
    }
    if(expansions >= 5){
        expansion[,5] = cos(6*pi*x)
    }      
   
    return(expansion)
            
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
