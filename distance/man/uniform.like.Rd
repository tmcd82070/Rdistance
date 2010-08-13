\name{uniform.like}
\alias{uniform.like}

\title{Uniform likelihood for distance analyses
}

\description{
Compute the uniform likelihood, scaled appropriately, for use as a likelihood in a distance analysis. 
}

\usage{
uniform.like(a, dist, w.lo = 0, w.hi = max(dist), series = "cosine", expansions = 0, scale = TRUE)
}

\arguments{
  \item{a}{Vector of parameter values. Length and meaning depend on \code{series} and \code{expansions}. 
        If no expansion terms were called for (i.e., \code{expansions = 0}), the distance likelihood is a 
        standard logistic function.  In this case, \code{a} needs to have  
        length 2, with \code{a[1]} being the upper end of support for the embedded uniform (i.e., likelihood is 
        approximately U[0,\code{a}], approximate because upper end declines smoothly to zero, see Details), 
        and \code{a[2]} is sharpness of the decline (or knee) at \code{a[1]}.
        If expansions are called for a[3:length(a)] are coefficients of the expansion terms.
  }
  \item{dist}{Numeric vector of observed distances.}
  \item{w.lo}{Scalar value of the lowest observable distance.  This is the \emph{left truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values less than \code{w.lo} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{w.hi}{Scalar value of the highest observable distance.  This is the \emph{right truncation} value for 
        sighting distances in \code{dist}.  Same units as \code{dist}.  Values greater than \code{w.hi} are allowed 
        in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output. 
  }
  \item{series}{A string specifying the type of expansion to use in computing the likelihood.  Currently, valid values are 
  'simple', 'hermite', and 'cosine'; but, see \code{\link{distance}} about defining other series.
  }
  \item{expansions}{A scaler specifying the number of terms in \code{series} to compute. Depending on the series, 
        this could be 0 through 5.  The default of 0 equates to no expansion terms of any type.}
  \item{scale}{Logical scaler indicating whether or not to scale the likelihood to 
        integrate to 1. This parameter is used to stop recursion in the computations.  If 
        \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) 
        is called, which in turn calls this likelihood function again with \code{scale} = FALSE. Thus, this 
        routine knows when its values are being used to compute the likelihood and when they 
        are being used to compute the constant of integration.  All user defined likelihoods must 
        have and use this parameter.}
}
\details{The 'uniform' likelihood used in \code{distance} is actually the \emph{logistic} function of the form, 
        \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}{f(x|a,b) = 
        1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-1))),}
        where \eqn{a} and \eqn{b} are parameters to be estimated.  Parameter \eqn{a} is the location of the 
        "discontinuity" or upper limit of an approximate uniform distribution's support.  This view is justified by 
        noting that 
        that \ifelselatex{\eqn{f^{-1}(0.5|a,b) = a}}{the inverse likelihood of 0.5 is \code{a}}
        Parameter \code{b} is the "knee" or sharpness of the bend at \code{a}.  This can be seen by noting that 
        \ifelselatex{\deqn{\frac{\partial}{\partial x} f(x|a,b)|_a = -b}}{slope of the likelihood at \eqn{a} is \eqn{-b}}. 
        If \eqn{b} is large, the "knee" is sharp at \eqn{a} and the likelihood looks like 
        a uniform distribution with support from \code{w.lo} to \eqn{a}.  If \eqn{b} is small, 
        the "knee" at \eqn{a} is not sharp and the likelihood declines in an elongated "S" shape.  See Examples 
        for plots for large and small values of \eqn{b}.    
        
        If \code{expansions} > 0, the appropriate expansion function is called 
        (see for example \code{\link{cosine.expansion}}).  Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} 
        expansion term for the \eqn{i^{th}}{i-th} distance and that \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)}
        are (estimated) coefficients for the expansion terms, the likelihood contribution for the \eqn{i^{th}}{i-th} 
        distance is, 
        \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}{f(x|a,b,c_1,c_2,...,c_k) 
        = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
        }
        
\value{ A numeric vector the same length as \code{dist} containing the likelihood contribution for 
        every value in \code{dist}.  Assuming \code{L=uniform(c(a,b),dist)}, the negative log likelihood for all distances is 
        \code{-sum(log(L), na.rm=T)}. Note that the returned value for distances less than \code{w.lo} or 
        greater than \code{w.hi} is \code{NA}.  Thus it is prudent to use \code{na.rm=TRUE}
        when computing the full likelihood of a data set.  

        If \code{scale} = TRUE, the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0.  
        If \code{scale} = FALSE, the integral of the likelihood is an arbitrary constant.  
}

\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{halfnorm.like)), \code{\link{hazrate.like}}, 
\code{\link{gamma.like}}, \code{\link{negexp.like}}
}
\examples{

x <- seq( 0, 100, length=100)

#   Plots showing effects of parameter a
plot(x, uniform.like(c(25,15), x))
plot(x, uniform.like(c(75,15), x))

#   Plots showing effects of parameter b
plot(x, uniform.like(c(50,20), x))
plot(x, uniform.like(c(50,0.5), x))

#   Plots showing likelihood showing effects of expansion terms
plot(x, uniform.like(c(50,20,3), x, expansions=1))
plot(x, uniform.like(c(50,20,3,-3), x, expansions=2))
plot(x, uniform.like(c(50,20,3), x, expansions=1, series="hermite"))


#   Estimate 'uniform' distance function
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]
dfunc <- F.dfunc.estim( x, likelihood="uniform", w.hi = 100)
plot(dfunc)
}

\keyword{ models }

