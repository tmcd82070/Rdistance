\name{uniform.like}
\alias{uniform.like}
\alias{hazrate.like}
\alias{halfnorm.like}
\alias{negexp.like}

\title{Standard likelihood functions for distance analyses.}

\description{
These functions compute likelihood contributions for off-transect 
sighting distances, scaled appropriately, for use as a distance likelihood. 
}

\usage{
uniform.like(a, dist, w.lo = 0, w.hi = max(dist),
series = "cosine", expansions = 0, scale = TRUE)
halfnorm.like(a, dist, w.lo = 0, w.hi = max(dist),
series = "cosine", expansions = 0, scale = TRUE)
negexp.like(a, dist, w.lo = 0, w.hi = max(dist),
series = "cosine", expansions = 0, scale = TRUE)
hazrate.like(a, dist, w.lo = 0, w.hi = max(dist),
series = "cosine", expansions = 0, scale = TRUE)
}


\arguments{
  \item{a}{A vector of likelihood parameter values. Length and meaning depend on \code{series} and \code{expansions}. 
        If no expansion terms were called for (i.e., \code{expansions = 0}), the distance likelihoods contain 
        one or two canonical parameters (see Details).  
        If one or more expansions are called for, coefficients for the expansion terms follow coefficients 
        for the canonical parameters.  If \code{p} is the 
        number of canonical parameters, coefficients for the expansion terms are \code{a[(p+1):length(a)]}.
  }
  \item{dist}{A numeric vector containing the observed distances.
  }
  \item{w.lo}{Scalar value of the lowest observable distance.  This is the \emph{left truncation} of 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values less than \code{w.lo} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{w.hi}{Scalar value of the largest observable distance.  This is the \emph{right truncation} of 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values greater than \code{w.hi} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{series}{A string specifying the type of expansion to use.  Currently, valid values are 
        'simple', 'hermite', and 'cosine'; but, see \code{\link{F.dfunc.estim}} about defining other series.
  }
  \item{expansions}{A scalar specifying the number of terms in \code{series}. Depending on the series, 
        this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.
  }
  \item{scale}{Logical scaler indicating whether or not to scale the likelihood so it 
        integrates to 1. This parameter is used to stop recursion in other functions.  If 
        \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) 
        is called, which in turn calls this likelihood function again with \code{scale} = FALSE. Thus, this 
        routine knows when its values are being used to compute the likelihood and when its value is being 
        used to compute the constant of integration.  All user defined likelihoods must 
        have and use this parameter.
  }
}

\details{

    \bold{Uniform}: The uniform likelihood is not technically uniform, but can look similar to a 
        uniform if the data warrant.  The uniform likelihood is actuall a \emph{heavy side} 
        or \emph{logistic} function of the form, 
        \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}{f(x|a,b) = 
        1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-a))),}
        where \eqn{a} and \eqn{b} are the parameters to be estimated.  Parameter \eqn{a} can be thought of 
        as the location of the 
        "approximate upper limit" of an uniform distribution's support by  
        noting that 
        that the inverse likelihood of 0.5 is always \code{a}.
        Parameter \code{b} is the "knee" or sharpness of the bend at \code{a} and estimates 
        the degree to which observations decline at the outer limit of sightability.  Note that the 
        slope of the likelihood at \eqn{a} is \eqn{-b}. 
        If \eqn{b} is large, the "knee" is sharp at \eqn{a} meaning that observations are uniformly 
        distributed between 0 and \code{a}, but then cease relatively abruptly at \eqn{a}. In this case,  
        the likelihood looks much like 
        a uniform distribution with support from \code{w.lo} to \eqn{a}.  If \eqn{b} is small, 
        the "knee" at \eqn{a} is not sharp and the density of observations (and thus the likelihood) 
        declines in an elongated "S" shape pivoting at \code{a}.  See Examples 
        for plots using large and small values of \eqn{b}.    
        

    \bold{Half Normal}: The half-normal likelihood is 
        \deqn{f(x|a) = \exp(-x^2 / a^2)}{f(x|a) = exp(-x^2 / a^2)}
        where \eqn{a} is the standard error parameter to be estimated.  If \eqn{a} is small, width of the 
        half-normal is small and sightability declines rapidly as distance increases.  
        If \eqn{a} is large, width of the half-hormal
        is large and sightability may not decline much between \eqn{w.lo} and \eqn{w.hi}. 
        
    \bold{Negative Exponential}: The negative exponential likelihood is
        \deqn{f(x|a) = \exp(-ax)}{f(x|a) = exp( -a*x )}
        where \eqn{a} is a slope parameter to be estimated. 
        

    \bold{Hazard Rate}: The hazard rate likelihood is
        \deqn{f(x|a,b) = 1 - \exp(-(x/a)^(-b))}{f(x|a,b) = 1 - exp(-(x/a)^(-b))}
        where \eqn{a} is a variance parameter, and \eqn{b} is a slope parameter to be estimated. 

    \bold{Expansion Terms}: 
        If \code{expansions} = k (k > 0), the expansion function specified by \code{series} is called 
        (see for example \code{\link{cosine.expansion}}).  Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} 
        expansion term for the \eqn{i^{th}}{i-th} distance and that \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)}
        are (estimated) coefficients for the expansion terms, the likelihood contribution for the \eqn{i^{th}}{i-th} 
        distance is, 
        \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}{f(x|a,b,c_1,c_2,...,c_k) 
        = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
        
}
        
\value{ 
        All likelihood functions return a numeric vector the same length and order as \code{dist} 
        containing the likelihood contribution for 
        corresponding distances in \code{dist}.  Assuming \code{L} is the returned vector 
        from one of these functions, the full log likelihood of all the data 
        is \code{-sum(log(L), na.rm=T)}. Note that the returned likelihood value for distances less than \code{w.lo} or 
        greater than \code{w.hi} is \code{NA}, and thus it is prudent to use \code{na.rm=TRUE}
        in the sum.  

        If \code{scale} = TRUE, the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0.  
        If \code{scale} = FALSE, the integral of the likelihood is an arbitrary.  
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}, 
\code{\link{Gamma.like}}
}

\examples{

\dontrun{

x <- seq( 0, 100, length=100)

#   ------ UNIFORM     
#   Plots showing effects of parameter a
plot(x, uniform.like(c(25,15), x))
plot(x, uniform.like(c(75,15), x))

#   Plots showing effects of parameter b
plot(x, uniform.like(c(50,20), x))
plot(x, uniform.like(c(50,0.5), x))

#   Plots showing effects of expansion terms
plot(x, uniform.like(c(50,20,3), x, expansions=1))
plot(x, uniform.like(c(50,20,3,-3), x, expansions=2))
plot(x, uniform.like(c(50,20,3), x, expansions=1, series="hermite"))

#   ------ HALF-NORMAL     
#   Plots showing effects of parameter a
plot(x, halfnorm.like(c(25), x))
plot(x, halfnorm.like(c(75), x))

#   Plots showing effects of expansion terms
plot(x, halfnorm.like(c(50,3), x, expansions=1))
plot(x, halfnorm.like(c(50,3,-3), x, expansions=2))
plot(x, halfnorm.like(c(50,3), x, expansions=1, series="hermite"))

#   ------ NEGATIVE EXPONENTIAL     
#   Plots showing effects of parameter a
plot(x, negexp.like(c(25), x))
plot(x, negexp.like(c(75), x))

#   Plots showing effects of expansion terms
plot(x, negexp.like(c(50,3), x, expansions=1))
plot(x, negexp.like(c(50,3,-3), x, expansions=2))
plot(x, negexp.like(c(50,3), x, expansions=1, series="hermite"))


#   ------ HAZARD RATE     
#   Plots showing effects of parameter a
plot(x, hazrate.like(c(25,25), x))
plot(x, hazrate.like(c(75,25), x))

#   Plots showing effects of parameter b
plot(x, hazrate.like(c(50,20), x))
plot(x, hazrate.like(c(50,0.5), x))

#   Plots showing effects of expansion terms
plot(x, hazrate.like(c(50,25,3), x, expansions=1))
plot(x, hazrate.like(c(50,25,3,-3), x, expansions=2))
plot(x, hazrate.like(c(50,25,3), x, expansions=1, series="hermite"))


#   ------ Estimate distance functions and plot
#   Generate half-norm data
set.seed(8383838)
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
un.dfunc <- F.dfunc.estim( x, likelihood="uniform", w.hi = 100)
hn.dfunc <- F.dfunc.estim( x, likelihood="halfnorm", w.hi = 100)
ne.dfunc <- F.dfunc.estim( x, likelihood="negexp", w.hi = 100)
hz.dfunc <- F.dfunc.estim( x, likelihood="hazrate", w.hi = 100)

par(mfrow=c(2,2))
plot(un.dfunc)
plot(hn.dfunc)
plot(ne.dfunc)
plot(hz.dfunc)
}
}

\keyword{ models }

