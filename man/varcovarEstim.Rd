% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varcovarEstim.R
\name{varcovarEstim}
\alias{varcovarEstim}
\title{Estimate variance-covariance}
\usage{
varcovarEstim(x, ml)
}
\arguments{
\item{x}{An estimated detection function object, normally 
produced by calling \code{\link{dfuncEstim}}.}

\item{ml}{Either a Rdistance 'model frame' or an Rdistance 
'fitted object'.  Both are of class "dfunc". 
Rdistance 'model frames' are lists containing components 
necessary to estimate a distance function, but no estimates. 
Rdistance 'model frames' are typically
produced by calls to \code{\link{parseModel}}. 
Rdistance 'fitted objects'
are typically produced by calls to \code{\link{dfuncEstim}}.
'Fitted objects' are 'model frames'
with additional components such as the parameters estimates, 
log likelihood value, convergence information, and the variance-
covariance matrix of the parameters.}
}
\value{
A square symmetric matrix estimating the 
variance-covariance matrix of parameters in \code{x}. 
Dimension of return is p X p, where p = \code{length(x$par)}.
}
\description{
Estimate the variance-covariance matrix of parameters 
in the distance function. If the likelihood is 
differentiable, the variance-covariance matrix is 
estimated from the second derivative of the likelihood 
(i.e., the hessian).  If the likelihood is not 
differentiable, the variance-covariance matrix is 
a matrix of 0's that are intepreted as "pending" (i.e., 
pending bootstrapping).
}
