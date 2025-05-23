% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrateNumeric.R
\name{integrateNumeric}
\alias{integrateNumeric}
\title{Numeric Integration}
\usage{
integrateNumeric(object, newdata = NULL)
}
\arguments{
\item{object}{An Rdistance model frame or fitted distance function,
normally produced by a call to \code{\link{dfuncEstim}}.}

\item{newdata}{A data frame containing new values for 
covariates at which either
ESW's or EDR's will be computed. If NULL and 
  \code{object} contains covariates, the  
  covariates stored in
  \code{object} are used (like \code{\link{predict.lm}}).
  If not NULL, covariate values in \code{newdata}
  are used. 
  See \bold{Value} section for more information.}
}
\value{
A vector of areas under distance functions. 
If \code{newdata} is specified, return length is 
\code{nrow(newdata)}.  If \code{newdata} is NULL, 
return length is \code{length(distances(object))}.
}
\description{
Numerically integrate under a distance function.
}
\section{Numeric Integration}{
 
Rdistance uses Simpson's composite 1/3 rule to numerically 
integrate distance functions from \code{object$w.lo} to 
\code{object$w.hi}. The number of points evaluated 
during numerical integration is controlled by 
\code{options(Rdistance_intEvalPts)} (default 101).
Option 'Rdistance_intEvalPts' must be odd because Simpson's rule
requires an even number of intervals. 
Lower values of 'Rdistance_intEvalPts' increase calculation speeds; 
but, decrease accuracy.
'Rdistance_intEvalPts' must be >= 5.  A warning is thrown if 
'Rdistance_intEvalPts' < 29. Empirical tests by the author 
suggest 'Rdistance_intEvalPts' values >= 30 are accurate 
to several decimal points for smooth distance functions
(e.g., hazrate, halfnorm, negexp)
and that all 'Rdistance_intEvalPts' >= 101 produce 
identical results if the distance function is smooth. 
  
\emph{Details}: Let \code{n} = \code{options(Rdistance_intEvalPts)}
and evaluate the distance function at \code{n} equal-spaced 
locations \{f(x0), f(x1), ..., f(xn)\}. 
Simpson's composite approximation to the area under the curve is
\deqn{\frac{1}{3}h(f(x_0) + 4f(x_1) + 2f(x_2) + 
     4f(x_3) + 2f(x_4) + ... + 2f(x_{n-2}) + 
     4f(x_{n-1}) + f(x_{n}))}{(1/3)h(f(x0) + 4f(x1) + 2f(x2) + 
     4f(x3) + 2f(x4) + ... + 2f(x(n-2)) + 4f(x(n-1)) + f(xn))}
where \eqn{h} is the interval size (w.hi - w.lo) / n.
}

\examples{

# Fake a distance function object
d <- units::set_units(rep(1,4),"m") # Only units needed, not values
obs <- factor(rep(c("obs1", "obs2"), 2))
ml <- list(
    mf = model.frame(d ~ obs) 
  , likelihood = "halfnorm"
  , expansions = 0
  , w.lo = units::set_units(0, "m")
  , w.hi = units::set_units(125, "m")
  , outputUnits = units(units::set_units(1,"m"))
  , transType = "line"
)
class(ml) <- "dfunc"
integrateNumeric(ml)

# Check:
w.hi <- 125
w.lo <- 0
s1 <- 40
s2 <- exp(log(s1) + log(0.5))
obs1Scaler <- (pnorm(w.hi, mean=w.lo, sd = s1) - 0.5) * sqrt(2*pi)*s1
obs2Scaler <- (pnorm(w.hi, mean=w.lo, sd = s2) - 0.5) * sqrt(2*pi)*s2
c(obs1Scaler, obs2Scaler)

}
