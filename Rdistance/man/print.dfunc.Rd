\name{print.dfunc}
\alias{print.dfunc}
\title{
    Print a distance function object.
}
\description{
    Print method for distance functions produced by F.dfunc.estim, 
    that are of class \code{dfund}.
}

\usage{
\method{print}{dfunc}(x, ...)
}

\arguments{
  \item{x}{
    An estimated distance function resulting from a call to \code{F.dfunc.estim}.
    }
  \item{\dots}{
    Included for compatability with other print methods.  Ignored here.
    }
}
\details{
    The call, coefficients of the distanced function, whether the estimation converged, 
    the likelihood and expansion function, and other statistics are printed.  At the bottom
    of the output, the following quantities are printed,
    \itemize{  
        \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
        \item \samp{Effective strip width} : Effective strip half-width as computed by \code{ESW}.
        \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
        Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
        \item \samp{Log likelihood} : Value of the maximized log likelihood.
        \item \samp{AIC} : Value of AIC for the distance function.
    }
    
    The number of digits printed is controled by \code{options()$digits}.
}
\value{
    The input value of \code{obj} is invisibly returned. 
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}




\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{plot.dfunc}}, \code{\link{print.abund}}
}

\examples{
\dontrun{
    set.seed(12345)
    x <- rgamma(1000, 2,2) 
    fit <- F.dfunc.estim(x, likelihood="Gamma")
    print(fit)
}
}
\keyword{ models }
