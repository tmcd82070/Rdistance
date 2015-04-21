\name{print.abund}
\alias{print.abund}
\title{
    Print abundance estimates.
}
\description{
    Print an object of class c("abund","dfunc") that is output 
    by \code{F.abund.estim}. 
}

\usage{
\method{print}{abund}(x, ...)
}

\arguments{
  \item{x}{
    An object output by \code{F.abund.estim}.  This is a distance function object that 
    also contains abundance estimates, and has class c("abund", "dfunc"). 
    }
  \item{\dots}{
    Included for compatibility to other print methods.  Ignored here.
    }
}

\details{
    The default print method for class 'dfunc' is called, then the abundance estimates 
    contained in \code{obj} are printed. 
}
\value{
    No value is returned.
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}



\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{F.abund.estim}}
}

\examples{
\dontrun{
    set.seed(444443)
    x <- rgamma(1000, 2,2) 
    fit <- F.dfunc.estim(x, likelihood="Gamma")
    fit <- F.abund.estim(fit, tot.trans.len=1000 )
    print(fit)
}  
}
\keyword{ models }

