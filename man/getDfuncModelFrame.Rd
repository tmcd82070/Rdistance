% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDfuncModelFrame.r
\name{getDfuncModelFrame}
\alias{getDfuncModelFrame}
\title{Return model frame for dfunc}
\usage{
getDfuncModelFrame(formula, data)
}
\arguments{
\item{formula}{A dfunc formula object.  See \code{dfuncEstim}.}

\item{data}{The data frame from which variables in formula (potentially) come.}
}
\value{
a model frame containing the response and covariates resulting from 
evaluating formula in data.
}
\description{
Returns the model frame from a formula and 
data set. This routine is intended to only be called from within other Rdistance
functions.
}
\details{
This routine is needed to get the scoping correct in \code{dfuncEstim}. 
In \code{dfuncEstim}, we first merge the detection and site data frames, then 
call this routine.
}
