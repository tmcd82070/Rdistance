\name{F.dfunc.estim}
\alias{F.dfunc.estim}

\title{
Estimate a detection function from distance-sampling data.
}
\description{
Fit a specific detection function to a set of observed off-transect distances. 
}

\usage{
F.dfunc.estim (dist, likelihood = "halfnorm", w.lo = 0, w.hi = max(dist), 
                            expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
                            observer = "both", warn = TRUE)
}

\arguments{
  \item{dist}{
    The vector of observed off-transect distances.  Alternatively, a data.frame with observed off-transect distances stored in a column named 'dist'.  See \code{\link{perp.dists}} to calculate off-transect distances from sighting angle and sighting distance.
}
  \item{likelihood}{
    String specifying the likelihood to assume. Valid values at present are "uniform", 
    "halfnorm", "hazrate", "negexp", and "Gamma".
}
  \item{w.lo}{
    Lower or left-truncation limit of the distances in \code{dist}.  This is the minimum 
    possible off-transect distance. Default is 0.
}
  \item{w.hi}{
    Upper or right-truncation limit of the distances in \code{dist}. This is the maximum 
    off-transect distance that could be observed. Default is the maximum of \code{dist}.
}  
  \item{expansions}{A scalar specifying the number of terms in \code{series} to compute. Depending on the series, this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.
}
\item{series}{
    If \code{expansions} > 0, this string specifies the type of expansion to use.
    Valid values at present are 'simple', 'hermite', and 'cosine'. 
}
\item{x.scl}{
    This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
}
\item{g.x.scl}{
    This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
}
\item{observer}{
    This parameter is passed to \code{F.g.estim}. See \code{F.gx.estim} documentation for definition.
}
\item{warn}{
    A logical scaler specifying whether to issue an R warning if the estimation did not converge or if one or more parameter estimates are at their boundaries.  For estimation, \code{warn} should generally be left at its default value of \code{TRUE}.  When computing bootstrap confidence intervals, setting \code{warn = FALSE} turns off annoying warnings when an iteration does not converge.  Regardless of \code{warn}, messages about convergence and boundary conditions are printed by \code{print.dfunc}, \code{print.abund}, and \code{plot.dfunc}, so there should be little harm in setting \code{warn = FALSE}.
}


}
\details{
Given a specified likelihood (e.g., "halfnorm"), maximum likelihood is used to estimate the parameter(s) of that likelihood function that best fit the distance data.  When the resulting detection function is plotted (see Examples), bins are plotted behind the detection function for visualization; however, the function is fit to the actual data, not to the bins.
}



\value{
An object of class 'dfunc'.  Objects of class 'dfunc' are lists 
containing the following components:
    \item{parameters}{The vector of estimated parameter values. 
        Length of this vector for built-in likelihood is the 
        number of expansion terms plus 1 plus 1 if the likelihood is 
        either 'hazrate' or 'uniform'. }
    \item{loglik}{The maximized value of the log likelihood (more specifically, the minimized value
of the negative log likelihood).}
    \item{convergence}{The convergence code. This code is returned by 
        \code{nlminb}.  Values other than 0 indicate suspect 
        convergence.}
    \item{like.form}{The form of the likelihood assumed. This is 
        the value of the argument \code{likelihood}. }
    \item{w.lo}{Left-truncation value used during the fit.}
    \item{w.hi}{Right-truncation value used during the fit.}
    \item{dist}{The input vector of observed off-transect distances.}
    \item{expansions}{The number of expansion terms used during estimation.}
    \item{series}{The type of expansion used during estimation.}
    \item{call}{The original call of this function.}
    \item{fit}{The fitted object returned by \code{nlminb}.  See documentation 
        for \code{nlminb}.}
}


\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
    
    Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}
}


\seealso{
See \code{\link{uniform.like}} for details on the "uniform", "halfnorm", "hazrate", and "negexp" likelihoods.  See \code{\link{Gamma.like}} for "Gamma" and \code{\link{powerexp.like}} for "powerexp".  And see package vignettes for information on
custom, user-defined likelihoods.  See also \code{\link{F.abund.estim}}, \code{\link{F.automated.CDA}} 
}

\examples{
# Load the example dataset of sparrow detections from package
data(sparrow.detections)

# Fit multiple detection functions to perpendicular, off-transect distances
un.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="uniform", w.hi = 150)
hn.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="halfnorm", w.hi = 150)
ne.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="negexp", w.hi = 150)
hz.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="hazrate", w.hi = 150)
ga.dfunc <- F.dfunc.estim(sparrow.detections, likelihood="Gamma", w.hi = 150, x.scl="max") 

# Plot the first four detection functions
par(mfrow=c(2,2))
plot(un.dfunc)
plot(hn.dfunc)
plot(ne.dfunc)
plot(hz.dfunc)
}

\keyword{ model }

