% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.dfunc.R
\name{summary.dfunc}
\alias{summary.dfunc}
\title{Summarize a distance function object}
\usage{
\method{summary}{dfunc}(object, criterion = "AICc", ...)
}
\arguments{
\item{object}{An Rdistance model frame or fitted distance function,
normally produced by a call to \code{\link{dfuncEstim}}.}

\item{criterion}{A string specifying the model fit criterion to print.
Must be one of "AICc" (the default), 
"AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas.}

\item{\dots}{Included for compatibility with other print methods.  Ignored here.}
}
\value{
The input distance function object (\code{object}), invisibly, 
with the following additional components:
\itemize{
  \item \code{convMessage}: The convergence message. If the distance function
  is smoothed, the convergence message is NULL.
  \item \code{effDistance}: The ESW or EDR.
  \item \code{pDetect}: Probability of detection in the strip.
  \item \code{AIC}: AICc, AIC, or BIC of the fit, whichever was requested.
  \item \code{coefficients}: If the distance function has coefficients, this 
  is the coefficient matrix with standard errors, Wald Z values, and p values. 
  If the distance function is smoothed, it has no coefficients and this component 
  is NULL.
}
}
\description{
A summary method for distance functions. 
Distance functions are produced by 
\code{dfuncEstim} (class \code{dfunc}).
}
\details{
This function prints the following 
quantities:
\itemize{  
  \item \samp{Call} : The original function call.
  \item \samp{Coefficients} : A matrix of estimated coefficients, their 
  standard errors, and Wald Z tests.
  \item \samp{Strip} : The left (\code{w.lo}) and right (\code{w.hi}) truncation values.
  \item \samp{Effective strip width or detection radius} : ESW or EDR as computed by \code{effectiveDistance}.
  \item \samp{Probability of Detection} : Probability of detecting a single target in the strip. 
  \item \samp{Scaling} : The horizontal and vertical coordinates used to scale the distance function. 
  Usually, the horizontal coordinate is 0 and the vertical coordinate is 1 (i.e., g(0) = 1).
  \item \samp{Log likelihood} : Value of the maximized log likelihood.
  \item \samp{Criterion} : Value of the specified fit criterion (AIC, AICc, or BIC).
}
The number of digits used in the printout is 
controlled by \code{options()$digits}.
}
\examples{
# Load example sparrow data (line transect survey type)
data(sparrowDf)

# Fit half-normal detection function
dfunc <- sparrowDf |> dfuncEstim(formula=dist~1)

# Print results
summary(dfunc)
summary(dfunc, criterion="BIC")

}
\seealso{
\code{\link{dfuncEstim}}, \code{\link{plot.dfunc}}, 
\code{\link{print.abund}}, \code{\link{print.abund}}
}
\keyword{models}
