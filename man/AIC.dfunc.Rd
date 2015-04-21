\name{AIC.dfunc}
\alias{AIC.dfunc}

\title{AICc and related fit statistics for detection function objects}

\description{
Computes AICc, AIC, or BIC for estimated distance functions.
}

\usage{
\method{AIC}{dfunc}(object, \dots, k = 2, n=length(object$dist))
}


\arguments{
  \item{object}{An estimated detection function object.  An estimated detection 
  function object has class 'dfunc', and is usually produced by a call to 
  \code{F.dfunc.estim}. }
  
  \item{\dots}{Required for compatability with the general \code{AIC} method.  Any 
  extra arguments to this function are ignored. }

  \item{k}{Scalar penalty to use in the computations.  See Details. }

  \item{n}{Scalar sample size to use in computations.  See Details. }


}

\details{
Regular Akaike's information criterion 
(\url{http://en.wikipedia.org/wiki/Akaike_information_criterion}) (\eqn{AIC}) is 
\deqn{AIC = LL + 2p,}{AIC = (LL) + 2p,}
where \eqn{LL} is the maximized value of the log likelihood (more specifically, the minimized value
of the negative log likelihood) and \eqn{p} is the 
number of coefficients estimated in the detection function.  For 
\code{dfunc} objects, \eqn{AIC} = \code{obj$loglik + 2*length(coef(obj))}.  A correction 
for small sample size, \eqn{AIC_c}{AICc}, is 
\deqn{AIC_c = LL + 2p + \frac{2p(p+1)}{n-p-1},}{AIC_c = LL + 2p + (2p(p+1))/(n-p-1),} 
where \eqn{n} is sample 
size or number of detected groups for distance analyses.  By default, this function 
computes \eqn{AIC_c}{AICc} because it converges to \eqn{AIC} for large \eqn{n} and is therefore generally prefered.   

By changing the parameters \code{k} and \code{n}, it is possible to compute at least
three measures of model fit.  These are: 
\itemize{
\item Setting \code{k} = 2 and \code{n} = \code{Inf} produces AIC.
\item Setting \code{k} = log(\eqn{n}) and \code{n} = \code{Inf}
    produces the Bayesian Information Criterion, or BIC.
\item Setting \code{k} = 2 and \code{n} = \eqn{n} produces \eqn{AIC_c}{AICc} (the default).
}     

}

\value{
A scalar. By default, the value of AICc for the estimated distance funciton \code{obj}.
}

\references{
Burnham, K. P., and D. R. Anderson, 2002. \emph{Model Selection and Multimodel Inference: 
    A Practical Information-Theoretic Approach, 2nd ed.} Springer-Verlag. ISBN 0-387-95364-7.

McQuarrie, A. D. R., and Tsai, C.-L., 1998. \emph{Regression and Time Series Model Selection.} 
    World Scientific. ISBN 981023242X    
    
    
}
\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{coef}}, \code{\link{F.dfunc.estim}}
}
\examples{
# Load the example dataset for sparrow detections from package
data(sparrow.detections)

# Compute perpendicular, off-transect distances from the observer's sight distance and angle
sparrow.detections$dist <- perp.dists(obs.dist=sparrow.detections$sightdist,
                                  obs.angle=sparrow.detections$sightangle)

# Fit detection function object
dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)

# Compute fit statistics
AIC(dfunc)  # AICc
AIC(dfunc, k=2, n=Inf)  # AIC
AIC(dfunc, k=log(length(dfunc$dist)), n=Inf)  # BIC
}

\keyword{ model }

