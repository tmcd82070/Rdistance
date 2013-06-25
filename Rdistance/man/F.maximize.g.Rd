\name{F.maximize.g}
\alias{F.maximize.g}
\title{
F.maximize.g - Finds the coordinate of the maximum of a distance function.
}

\description{
Find the x coordinate that maximizes g(x).
}

\usage{
F.maximize.g( fit )
}

\arguments{
  \item{fit}{
  An estimated 'dfunc' object produced by \code{F.dfunc.estim}.
  }
}


\value{
The value of x that maximized g(x) in \code{fit}.
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
\dontrun{
#   Fake data
set.seed(22223333)
x <- rgamma(100, 10, 1)

fit <- F.dfunc.estim( x, likelihood="Gamma", x.scl="max" )

F.maximize.g( fit )  # should be near 10.
fit$x.scl            # same thing
}
}
\keyword{ model }

