\name{distance-package}
\Rdversion{1.1}
\alias{distance-package}
\alias{distance}
\docType{package}

\title{Line Transect and Point Count Distance Analyzes}

\description{
Analysis of distance-based transect and point count data.  This package replicates 
analyses of program DISTANCE, and adds functionality to estimate non-monotonic density smoother
methods and gamma density based methods.
}

\details{
\tabular{ll}{
Package: \tab distance\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-10-26\cr
License: \tab GNU General Public License\cr
LazyLoad: \tab yes\cr
}

\emph{Overview:  }
Line transect and point count data are common in biological and ecological field studies.  In 
these studies, detection of the items of interest (i.e., the animals or plants) often declines 
as distance from an observer increases.  Studies where this is true are commonly called 'distance based'
studies.  Distance based studies are one of two common study types 
used to estimate population size in ecological field studies (the other is mark-recapture or capture-recapture
studies, see package \code{\link[mra:mra-package]{mra}}). Line transect studies are commonly used 
to estimate size of large mammal populations (e.g., bear, whale, ungulates, etc.).  Point count studies, where 
count stations are aligned along some route,  
are commonly used to estimate size of bird populations.  In addition, detection of the items of 
interest can be associated with extraneous covariates (such as habitat, weather, glare, etc.) in 
addition to declining with increasing distance. 

The data from line transect analyses consists of 
off-transect distances (possibly binned) and total transect lengths.  Data from point count studies consist of 
detection distances from sample locations and total number of locations.  From these data, it is possible 
to estimate a so-called \emph{detection function} that predicts probability of detection as a function of
other study variables.  Using this function, and the total amount of sampling that took place, as well 
as key assumptions in some cases, it is possible to estimate density of the items of interest in the 
sampled area.

The key functions of this package are \code{\link{F.transect}} and \code{\link{F.point}}.  These 
function accept a data frame containing one row per detected item and the following basic data columns: 

\enumerate{
\item Transect or Point ID
\item Observer ID: if a double-observer system was used (REFERENCE). 
\item Strata ID: if separate distance functions are to be fitted in separate strata. Strata might 
consist of left-side right-side designation, habitat types, observer name, etc.
\item Off-transect or off-point distance for every item detected on the transect or at the point.
\item Total transect length or total number of points in the route
\item Total study area or strata size: if abundance estimates are desired, otherwise density only 
is estimated.
}

Input parameters to \code{F.transect} and \code{F.point} specify the type of distance 
function to fit and other estimation parameters (such as whether to compute bootstrap 
confidence intervals). For a list of the types of distance functions that can be fitted, as 
well as actions of other parameters, see the documentation for \code{F.transect} or \code{F.point}. 

}

\author{
The following individuals contributed heavily to the contents of this package (in alphabetic order): 

Jim Griswald
Patrick McCann
Trent McDonald
Ryan Nielson

Maintainers: All of the above. 

Contact: Trent McDonald <tmcdonald@west-inc.com>
}

\references{
Buckland, S.T., D. R. Anderson, K. P. Burnham, J. L. Laake, D. L. Borchers, and L. Thomas. 
????. \emph{Distance Sampling}. 
Oxford University Press???.

Buckland, S.T., D. R. Anderson, K. P. Burnham, J. L. Laake, D. L. Borchers, and L. Thomas. 
2004. \emph{Advanced Distance Sampling: Estimating abundance of biological populations}. 
Oxford University Press. 
}

\seealso{
\code{\link[mra:mra-package]{mra}} for Mark-Recapture Analyses 
}

\examples{
fake.df <- data.frame( tranID=rep(1:10, each=10), distance=abs( rnorm(100, 0, 10)) )
fake.dfit <- F.transect( distance ~ 1, transect = tranID, data=fake.df )
print(fake.dfit)
}

\keyword{ package }
\keyword{ modeling }

