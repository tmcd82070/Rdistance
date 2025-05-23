% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/halfnorm.like.R
\name{halfnorm.like}
\alias{halfnorm.like}
\title{halfnorm.like - Half-normal distance function}
\usage{
halfnorm.like(a, dist, covars, w.hi = NULL)
}
\arguments{
\item{a}{A vector or matrix of covariate 
and expansion term 
coefficients. Dimension is $k$ X $p$, where 
$k$ (i.e., \code{nrow(a)}) is the number of coefficient
vectors to evaluate (cases) and $p$ (i.e., \code{ncol(a)})
is the number of covariate and expansion 
coefficients in the likelihood. If \code{a} is a 
dimensionless vector, it is interpreted to be a 
single row with $k$ = 1. 
Covariate coefficients in \code{a} are the first 
$q$ values ($q$ <= $p$), and must be on a log scale.}

\item{dist}{A numeric vector of length $n$ or 
a single-column matrix (dimension $n$X1) containing 
detection distances at which to evaluate the likelihood.}

\item{covars}{A numeric vector of length $q$ or 
matrix of dimension $n$X$q$ containing covariate values 
associated with distances in argument \code{d}}

\item{w.hi}{A numeric scalar containing maximum 
distance. The right-hand cutoff or upper limit.}
}
\value{
A list containing the following two components:
\itemize{
  \item \bold{L.unscaled}: A matrix of size $n$X$k$X$b$ 
   containing likelihood values evaluated at 
   distances in \code{dist}.
    Each row is associated with 
   a single distance, and each column is associated with 
   a single case (row of \code{a}).  This matrix is  
   "unscaled" because the underlying likelihood does 
   not integrate to one. Values in \code{L.unscaled} 
   are always greater than or equal to zero.
   
 \item \bold{params}: A $n$X$k$X$b$ array of the 
 likelihood's (canonical) parameters, First page contains 
 parameter values related to covariates (i.e., $s = exp(x'a)$),
 while subsequent pages contain other parameters. 
 $b$ = 1 for halfnorm, negexp; $b$ = 2 for hazrate and 
 others.
 Rows correspond to distances in \code{dist}. Columns 
 correspond to rows from argument \code{a}. 
}
}
\description{
Evaluate the half-normal distance function, 
for sighting distances, potentially including covariates
and expansion terms
}
\details{
The half-normal distance function is 
\deqn{f(d|s) = \exp(-d^2 / (2*s^2))}{f(d|s) = exp(-d^2 / (2*s^2))}
where \eqn{s = exp(x'a)}, \eqn{x} is a vector of 
covariate values associated with distance \eqn{d} 
(i.e., a row of \code{covars}), and 
\eqn{a} is a vector of the first $q$ (=\code{ncol(covars)}) 
values in argument \code{a}.

Some authors parameterize the halfnorm without 
the "2" in the denominator of the exponent. 
\code{Rdistance} includes 
"2" in this denominator to make 
quantiles of the half normal agree with 
the standard normal. This means that half-normal 
coefficients in 
Rdistance (i.e., $s = exp(x'a)$) can be 
interpreted as normal standard errors.  For example, 
approximately 95\% of distances should 
occur between 0 and 2$s$.
}
\examples{
 
d <- seq(0, 100, length=100)
covs <- matrix(1,length(d),1)
halfnorm.like(log(20), d, covs)

plot(d, halfnorm.like(log(20), d, covs)$L.unscaled, type="l", col="red")
lines(d, halfnorm.like(log(40), d, covs)$L.unscaled, col="blue")

# Matrix inputs:
d <- matrix(c(0,10,20), ncol = 1) # 3X1
covs <- matrix(c(rep(1,nrow(d)), rep(.5,nrow(d))), nrow = nrow(d)) # 3X2
coefs <- matrix(log(c(15,5,10,10)), nrow=2) # 2X2
L <- halfnorm.like( coefs, d, covs ) 
L$L.unscaled # 3X2
L$params     # 3X2; exp(log(15)+0.5log(10)) and exp(log(5)+0.5log(10))

}
\seealso{
\code{\link{dfuncEstim}},
         \code{\link{hazrate.like}},
         \code{\link{negexp.like}}
}
