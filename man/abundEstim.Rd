% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abundEstim.R
\name{abundEstim}
\alias{abundEstim}
\title{Estimate abundance from distance-sampling data.}
\usage{
abundEstim(dfunc, detectionData, siteData, area = 1, ci = 0.95, R = 500,
  plot.bs = FALSE, bySite = FALSE)
}
\arguments{
\item{dfunc}{An estimated 'dfunc' object produced by \code{dfuncEstim}.}

\item{detectionData}{A data.frame with each row representing one detection
(see example dataset, \code{\link{sparrowDetectionData}}) and with at least
the following three columns: 
\itemize{ 
  \item \code{siteID} = ID of the transect or point. 
  \item \code{groupsize} = the number of individuals in the detected group. 
  \item \code{dist} = the perpendicular, off-transect distance or radial
    off-point distance to the detected group. 
}}

\item{siteData}{A data.frame with each row representing one site 
(transect or point) (see example dataset, 
\code{\link{sparrowSiteData}}). If the data in \code{detectionData}
is from line transects, \code{siteData} must have at 
least the following two columns:
\itemize{ 
  \item \code{siteID} = ID of the transect or point. This vector 
  is used during bootstrapping to resample sites. 
  \item \code{length} = the length of the transect. 
}
If the data in \code{detectionData}
is from point transects, \code{siteData} must have a
\code{siteID} column only.  For both data types, \code{siteID} 
  is used during bootstrapping to resample sites.}

\item{area}{Total study area size.  If \code{area} = 1, density is estimated.
Density has units (number of animals) per (squared 
units of the distance measurements).  For example, if distance values fitted
in \code{dfunc} are meters, density is number of individuals per
square meter.  If distances are miles, density is number of
individuals per square mile.  If \code{area} > 1, total abundance on the
study area is estimated and units are (number of animals).  This can also
be used to convert units for density. For example, if distance values fitted
in \code{dfunc} are meters, and area is set to 10,000, density is number
of individuals per hectare (ha; 1 ha = 10,000 square meters).
square meter.}

\item{ci}{A scaler indicating the confidence level of confidence intervals. 
Confidence intervals are computed using the bias corrected bootstrap
method. If \code{ci = NULL}, confidence intervals are not computed.}

\item{R}{The number of bootstrap iterations to conduct when \code{ci} is not
NULL.}

\item{plot.bs}{A logical scalar indicating whether to plot individual
bootstrap iterations.}

\item{bySite}{A logical scalar indicating whether to compute site-level
estimates of abundance. The default (\code{bySite=FALSE}) returns only one
overall abundance estimate. This routine does not calculate confidence
intervals for these site-level abundance estimates, so \code{ci} is set to
\code{NULL} if \code{bySite = TRUE}. See \code{\link{estimateN}}.}
}
\value{
If \code{bySite} is FALSE, an 'abundance estimate' object, a list of
  class c("abund", "dfunc"), containing all the components of a "dfunc"
  object (see \code{dfuncEstim}), plus the following: 
  
 \item{abundance}{Estimated abundance in the study area (if \code{area} >
 1) or estimated density in the study area (if \code{area} = 1).}
  \item{n}{The number of detections
 (not individuals, unless all group sizes = 1) used in the estimate of
 abundance.}
  \item{area}{Total area of inference. Study area size}
  \item{esw}{Effective strip width for line-transects, effective
  radius for point-transects.  Both derived from \code{dfunc}}.
  \item{n.sites}{Total number of transects for line-transects, 
  total number of points for point-transects.}
  \item{tran.len}{Total transect length. NULL for point-transects.}
  \item{avg.group.size}{Average group size}
  \item{ci}{The bias corrected bootstrap confidence interval for
  \code{n.hat}.  The names of this component give the quantiles of the
  bootstrap distribution used to compute the bias corrected interval.} 
  \item{B}{A vector or length \code{R} containing all bootstrap estimated
  population sizes. If a particular interation did not converge, the
  corresponding entry in \code{B} will be \code{NA}. The bootstrap
  distribution of \code{n.hat} can be plotted with \code{hist(x$B)}, where
  \code{x} is an 'abundance estimate' object. The confidence interval in
  \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)],
  p=names(x$ci) )}.}
  \item{alpha}{The (scalar) confidence level of the
  confidence interval for \code{n.hat}.} 
  
  If \code{bySite} is TRUE, a data frame containing site-level 
  estimated abundance.  The data frame is an exact copy of \code{siteData}
  with the following columns tacked onto the end:
   
  \item{effDist}{The effective sampling distance at the site.  For line-
  transects, this is ESW at the site.  For points, this is EDR. } 
  \item{pDetection}{Average probability of deteciton at the site. 
  If only site-level covariates appear in the distance function, 
  pDetection is constant within a site. When detection-level 
  covariates are present, pDetection is the average at the site.}
  \item{observedCount}{The total number of individuals detected at a site.}
  \item{abundance}{Estimated abundance at the site. This is the sum
  of inflated group sizes at the site. i.e., each group size 
  at the site is divided by its pDetection, and then summed.    }
  \item{density}{Estimated density at the site. This is abundance 
  at the site divided by the sampled area at the site.  E.g., for 
  line transects, this is abundance divided by \eqn{2*w*length}. For 
  points, this is abundance divided by \eqn{pi*w^2}.}
  \item{effArea}{The effective area sampled at the site. This could be used
  as an offset in a subsequent linear model. For 
  line transects, this is \eqn{2*ESW*length}. For 
  points, this is \eqn{pi*EDR^2}.}
}
\description{
Estimate abundance (or density) given an estimated detection
  function and supplemental information on observed group sizes, transect
  lengths, area surveyed, etc.  Also computes confidence intervals of
  abundance (or density) using the bias corrected bootstrap method.
}
\details{
The abundance estimate is \deqn{N =
  \frac{n.indiv(area)}{2(ESW)(tot.trans.len)}}{N = n.indiv*area /
  (2*ESW*tot.trans.len)} where n.indiv is either \code{avg.group.size * n} or
  \code{sum(group.sizes)}, and \code{ESW} is the effective strip width
  computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
  
  The confidence interval for abundance assumes that the fundamental units of
  replication (transects) are independent. The bias corrected bootstrap
  method used here resamples the units of replication (transects) and
  recalculates the model's parameter estimates.  If a double-observer data
  frame is included in \code{dfunc}, rows of the double-observer data frame
  are re-sampled each bootstrap iteration. No model selection is performed.
  By default, \code{R} = 500 iterations are performed, afterwhich the bias
  corrected confidence intervals are computed using the method given in Manly
  (1997, section 3.4).
}
\examples{
# Load the example datasets of sparrow detections and transects from package
  data(sparrowDetectionData)
  data(sparrowSiteData)
  
  # Fit detection function to perpendicular, off-transect distances
  dfunc <- dfuncEstim(sparrowDetectionData, w.hi=150)
  
  # Estimate abundance given a detection function
  # Note, area=10000 converts to density per hectare (for distances measured in meters)
  # Note, a person should do more than R=20 iterations 
  fit <- abundEstim(dfunc, detectionData=sparrowDetectionData, 
         siteData=sparrowSiteData, area=10000, R=20, ci=0.95, 
         plot.bs=TRUE, by.id=FALSE)
         
}
\references{
Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and monte
  carlo methods in biology}, London: Chapman and Hall.
  
  Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
   and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
   abundance of biological populations}. Oxford University Press, Oxford, UK.
}
\seealso{
\code{\link{dfuncEstim}}
}
\author{
Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr 
  Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}\cr 
  Jason Carlisle, University of Wyoming and WEST Inc., 
  \email{jcarlisle@west-inc.com}
}
\keyword{model}