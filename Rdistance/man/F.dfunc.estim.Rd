\name{F.dfunc.estim}
\alias{F.dfunc.estim}

\title{
F.dfunc.estim - Estimate a distance function.
}
\description{
Fit a specific distance function to a set of observed off-transect distances. 
}

\usage{
F.dfunc.estim(dist, likelihood = "halfnorm", w.lo = 0, w.hi = max(dist), expansions = 0, 
    series = "cosine", x.scl=0, g.x.scl=1, observer="both", warn=TRUE )
}

\arguments{
  \item{dist}{
    The vector of observed off-transect distances. All values in \code{dist} must be 
    between \code{w.lo} and \code{w.hi}. 
}
  \item{likelihood}{
    String specifying the likelihood to assume. Valid values at present are "uniform", 
    "halfnorm", "hazrate", "negexp", and "Gamma". See Details and Example for user defined likelihoods.
}
  \item{w.lo}{
    Lower or left-truncation limit of the distances in \code{dist}.  This is the minimum 
    possible off-transect distance. Default is 0.
}
  \item{w.hi}{
    Upper or right-truncation limit of the distances in \code{dist}. This is the maximum 
    off-transect distance that could be observed. Default is the maximum of \code{dist}.
}  
  \item{expansions}{A scalar specifying the number of terms in \code{series} to compute. Depending on the series, 
    this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.
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
    A logical scaler specifying whether to issue an R warning if the estimation did not converge or if one or more parameter 
    estimates are at their boundaries.  For estimation, \code{warn} should generally be left at its default value of \code{TRUE}.  When computing 
    bootstrap confidence intervals, setting \code{warn = FALSE} turns off annoying warnings when an iteration does not
    converge.  Regardless of \code{warn}, messages about convergence and boundary conditions are printed by \code{print.dfunc}, \code{print.abund},
    and \code{plot.dfunc}, so there should be little harm in setting \code{warn = FALSE}.
}


}
\details{
\code{Rdistance} allows user-defined likelihood functions (see Examples).  Assuming the user defined likelihood is named <\emph{form}>,
estimation of a user defined likelihood 
requires that the following two functions be defined: 

\enumerate{
\item <\emph{form}>.like - The likelihood:  This function defines the user-defined likelihood and should take the following inputs, in this order:
    \itemize{
        \item \code{a} = the parameter vector for the likelihood.
        \item \code{dist} = the vector of observed distances.
        \item \code{w.lo} = the left truncation (minimum distance).
        \item \code{w.hi} = the right truncation (maximum distance).
        \item \code{series} = the name of the expansion series.  If the likelihood does not use a series, this 
            function still requires a parameter named \code{series}. 
        \item \code{expansions} = the number of expansions.  If the likelihood does not use expansions, this 
            function still needs a parameter named \code{expansions}. 
        \item \code{scale} = a logical scalar.  If TRUE, the likelihood should be scaled to integrate to 1.0.  If FALSE, the 
            user defined likelihood does not need to integrate to 1.0.  See \code{uniform.like} for an example of how this 
            parameter should be used.
    }
    The likelihood function should return a vector the same length as \code{dist} containing the likelihood values. That is, the i-th 
    element of the output vector should be the likelihood of observing \code{dist[i]}. 

\item <\emph{form}>.start.limits - The starting values, limits, and names of parameters in the likelihood.  This function  
    takes the arguments \code{dist}, \code{w.lo}, and \code{w.hi}, and returns a list containing the following components: 
    \itemize{
        \item \code{start} = a vector of length \emph{p} of starting values for the parameters of the likelihood, assuming 
            there are \emph{p} parameters in the likelihood. 
        \item \code{lowlimit} = a vector of length \emph{p} of lower limits for parameters of the likelihood. 
        \item \code{highlimit} = a vector of length \emph{p} of upper limits for parameters of the likelihood.  
        \item \code{names} = a vector of length \emph{p} of names for the parameters of the likelihood. 
    }
}

}

\value{
An object of class 'dfunc'.  Objects of class 'dfunc' are lists 
containing the following components:
    \item{parameters}{The vector of estimated parameter values. 
        Length of this vector for built-in likelihood is the 
        number of expansion terms plus 1 plus 1 if the likelihood is 
        either 'hazrate' or 'uniform'. }
    \item{loglik}{The maximized value of the log likelihood. }
    \item{convergence}{The convergence code. This code is returned by 
        \code{nlminb}.  Values other than 0 indicate suspect 
        convergence.}
    \item{like.form}{The form of the likelihood assumed. This is 
        the value of the argument \code{likelihood}. }
    \item{w.lo}{Left-truncation value used during the fit.}
    \item{w.hi}{Right-truncation value used during the fit.}
    \item{dist}{The input vector of observed off-transect distances.}
    \item{expansions}{The number of expansion terms used during estimation.}
    \item{series}{The number of expansion terms used during estimation.}
    \item{call}{The original call of this function.}
    \item{fit}{The fitted object returned by \code{nlminb}.  See documentation 
        for \code{nlminb}.}
}


\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
Likelihoods listed in \code{\link{uniform.like}}; 
\code{\link{F.abund.estim}}; \code{\link{F.automated.CDA}} 
}

\examples{

\dontrun{
#   ------ Fit distance functions to half-normal data
set.seed(90382)
x <- rnorm(1000) * 20
x <- x[ 0 < x & x < 100 ]

un.dfunc <- F.dfunc.estim( x, likelihood="uniform", w.hi = 100)
hn.dfunc <- F.dfunc.estim( x, likelihood="halfnorm", w.hi = 100)
ne.dfunc <- F.dfunc.estim( x, likelihood="negexp", w.hi = 100)
hz.dfunc <- F.dfunc.estim( x, likelihood="hazrate", w.hi = 100)
ga.dfunc <- F.dfunc.estim( x, likelihood="Gamma", w.hi = 100, x.scl="max") 

par(mfrow=c(2,2))
plot(un.dfunc)
plot(hn.dfunc)
plot(ne.dfunc)
plot(hz.dfunc)


#   ------ A user defined likelihood function: the triangular distribution on [0,b]
triangular.like <- function(b, dist, w.lo, w.hi, series = "", expansions = 0, scale = TRUE){
    L <- (2/b)*(1 - dist/b)
    L[ L < 0 ] <- 0
    L 
}

triangular.start.limits <- function(dist, w.lo, w.hi){
    list(start=max(dist)*.75,
         lowlimit=w.lo,
         highlimit=w.hi,
         names="Max")
}

#   A function to generate triangular random deviates
rtriang <- function(n, b){
    x<-seq(0,b,length=500)
    CDF <- 2*x/b - (x/b)^2
    u <- runif(n)
    r <- approx( CDF, x, xout=u )$y
}

d <- rtriang(500,100)  # true b = 100
tri.dfunc <- F.dfunc.estim( d, likelihood="triangular", w.hi=150 )
plot(tri.dfunc)

#   For triangular case, true effective strip width = tri.dfunc$g.x.scl*tri.dfunc$param / 2.  
#   ESW(tri.dfunc) may differ slightly due to numerical integration error.
}
}

\keyword{ model }

