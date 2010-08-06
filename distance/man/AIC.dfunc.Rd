\name{AIC.dfunc}
\alias{AIC.dfunc}

\title{Return AIC for distance function objects}

\description{
Computes AICc, AIC, or BIC for estimated distance functions.
}

\usage{
AIC.dfunc(obj, k = 2, n=length(obj$dist), ...)
}


\arguments{
  \item{obj}{An estimated distance function object.  An estimated distance 
  function object has class 'dfunc', and is usually produced by a call to 
  \code{F.dfunc.estim}. }
  
  \item{k}{Scalar penalty to use in the computations.  See Details. }

  \item{n}{Scalar sample size to use in computations.  See Details. }

  \item{\dots}{Required for compatability with the AIC method.  Any 
  extra arguments to this function are ignored. }

}

\details{
Regular Akaike's information criterion 
(\link{http://en.wikipedia.org/wiki/Akaike_information_criterion}) (AIC) is 
(LL) + 2p, where LL is the maximized value of the log likelihood and p is the 
number of coefficients estimated in the distance function.  For 
\code{dfunc} objects, AIC = \code{obj$loglik + 2*length(coef(obj))}.  A correction 
for small sample size (AICc) is (LL) + 2p + (2p(p+1))/(n-p-1), where n is sample 
size or number of sighted groups for distance analyses.  By default, this function 
computes AICc because it converges to AIC for large n and is therefore generally 
prefered.   

By changing the parameter \code{k} and \code{n}, it is possible to compute at least
three measures of model fit.  These are: 
\enumerate{
\item Setting \code{k} = 2 and \code{n} = \code{Inf} produces AIC
\item Setting \code{k} = log(n) and \code{n} = \code{Inf}
    produces the Bayesian Information Criterion, or BIC.
\item Setting \code{k} = 2 and \code{n} = n produces AICc (the default).
}     

}

\value{
A scalar.  By default, the value of AICc for the estimated distance funciton \code{obj}.
}

\references{
Burnham, K. P., and D. R. Anderson, 2002. \emph{Model Selection and Multimodel Inference: 
    A Practical Information-Theoretic Approach, 2nd ed.} Springer-Verlag. ISBN 0-387-95364-7.

McQuarrie, A. D. R., and Tsai, C.-L., 1998. \emph{Regression and Time Series Model Selection.} 
    World Scientific. ISBN 981023242X    
    
    
}
\author{
Trent McDonald, WEST Inc.; tmcdonald@west-inc.com
}


\seealso{
\code{\link{coef}}, \code{\link{F.dfunc.estim}}
}
\examples{
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
fit <- F.dfunc.estim(x, w.lo=0, w.hi=100)
AIC(fit)  # AICc
AIC(fit, k=2, n=Inf)  # AIC
AIC(fit, k=length(fit$dist), n=Inf)  # BIC
}

\keyword{ model }

