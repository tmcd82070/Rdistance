\name{GoF.GMRDS}
\alias{GoF.GMRDS}

\title{Goodness-of-Fit Testing of a Gamma-Shaped Detection Function}
\description{
 Calculates Chi-square and Kolmogorov goodness-of-fit tests of a gamma-shaped detection model against observations
}
\usage{
GoF.GMRDS(FittedModel, USE.KS = TRUE)
}
\arguments{
  \item{FittedModel}{object returned by \code{\link{FitGMRDSmodel}}}
  \item{USE.KS}{logical, if \code{TRUE} also calculates Kolmogorov test.}
}

\details{
The fitted gamma-shaped detection model is used to transform the observations to a sample from the Uniform [0,1], assuming the model is an adequate description.  Goodness-of-Fit to the Uniform is tested in two ways:
\enumerate{
\item Using a Chi-squared test, after binning the observations into 15 equal-sized bins.  Histograms are created of each observer's binned, transformed observations.
\item Using a Kolmogorov test, thus retaining information on the continuous distribution of observations.}

If the sample size is < 100, an exact P value is calculated for the Kolmogorov test.\cr
}
\value{
  The histograms are created in a graphics window and the test summaries printed to the command window.
}
\section{Warning}{The Chi-square test can be very dependent on the binning.\cr

The reported P value from the Kolmogorov test will be conservative given that the same observations being tested were used to fit the model.}

\references{ \cite{Conover, W. J., 1999, \emph{Practical Nonparametric Statistics}, 3 ed.  Wiley, NY, NY.}}
\author{ P. X. Quang,\cr E. F. Becker \email{earl.becker@alaska.gov},\cr J. H. Reynolds \email{Joel\_reynolds@fws.gov}}

\seealso{\code{\link{FitGMRDSmodel}},  \code{\link[stats]{ks.test}}}

\examples{
## using the Togiak NWR example data

 GoF.GMRDS(TogiakModel,USE.KS=TRUE)

## Pilot Observations =  133 
## Passenger Observations =  129 
## Total Observations =  177 
## Pilot ChiSq =  19.14286 ; Pvalue =  0.03848525 
## Passenger ChiSq =  20.88372 ; Pvalue =  0.02191914  
##
## Results from the Kolmogorov test for Pilot:  
##               test statistic =  0.09188414  and p-value =  0.2114437 . 
## Results from the Kolmogorov test for Passenger:  
##               test statistic =  0.09040255  and p-value =  0.2423965 . 

# also creates histograms...
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

\keyword{htest}
