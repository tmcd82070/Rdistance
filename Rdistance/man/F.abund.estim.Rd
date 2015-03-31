\name{F.abund.estim}
\alias{F.abund.estim}
\title{
Estimate abundance from distance-sampling data.
}

\description{
Estimate abundance (or density) given an estimated detection function and supplemental information on observed group sizes, transect lengths, area surveyed, etc.  Also computes confidence intervals of abundance (or density) using the bias corrected bootstrap method.
}

\usage{
F.abund.estim(dfunc, distdata, covdata, area = 1, 
    ci=0.95, R=500, bs.method="transects", plot.bs=FALSE)
}

\arguments{
  \item{dfunc}{
    An estimated 'dfunc' object produced by \code{F.dfunc.estim}.
    }

  \item{distdata}{
  A data.frame where each row represents one detection (see Vignette and example sparrow dataset) and with at least the following three columns with the names \code{siteID}, \code{groupsize}, and \code{dists}:
  \itemize{
        \item \code{siteID} = the name of the transect.
        \item \code{groupsize} = the number of individuals in the detected group.
        \item \code{dists} = the off-transect distance.
        }
  }


  \item{covdata}{
  A data.frame where each transect is represented as one row (see Vignette and example sparrow dataset) and with at least the following two columns with the names \code{siteID} and \code{length}:
    \itemize{
        \item \code{siteID} = the name of the transect.  This vector is used during bootstrapping to resample transects.
        \item \code{length} = the length of the transect.
        }
  }
  
  
  \item{area}{
    Total study area size.  If \code{area} = 1, density is estimated. 
    Density has units (# of animals) per (z), where (z) is the square 
    units of the distance measurements.  For example, if distance values 
    fitted in \code{dfunc} were in meters, density will be number of individuals 
    per square meter.  If distances are miles, density will be number of individuals 
    per square mile.  If \code{area} > 1, total abundance on the study 
    area is estimated and units are (# of animals).
}

    
  \item{ci}{A scaler indicating the confidence level of confidence intervals.  
    Confidence intervals are computed using the bias corrected bootstrap method.  
    If \code{ci} = NULL, confidence intervals are not computed. 
    }
  
  \item{R}{The number of bootstrap iterations to conduct when \code{ci} is not NULL. 
    }
    
  \item{bs.method}{The units that are resampled during bootstrap iterations.
  Currently, the only option is 'transects', but future versions will allow the resampling of individual detections instead of transects.  See Details.
  }
    
  \item{plot.bs}{A logical scalar indicating whether to plot individual bootstrap iterations.}
}



\details{
The abundance estimate is 
\deqn{N = \frac{n.indiv*area}{2*ESW*tot.trans.len}}{N = n.indiv*area / (2*ESW*tot.trans.len)}
where n.indiv is either \code{avg.group.size * n} or \code{sum(group.sizes)}, and \code{ESW} 
is the effective strip width computed from the estimated distance function (i.e., \code{ESW(dfunc)}). 

The confidence interval for abundance assumes that the fundamental units of replication 
(transects or individual detections) are independent. The 
bias corrected bootstrap method used here resamples the units of replication and recalculates the 
model's parameter estimates.  If a double-observer data frame is included in \code{dfunc}, 
rows of the double-observer data frame are re-sampled each bootstrap iteration.  
No model selection is performed. By default, \code{R} = 500 
iterations are performed, afterwhich the bias corrected confidence intervals are computed 
using the method given in Manly (1997, section 3.4).  


}
\value{
An 'abundance estimate' object, a list of class c("abund", "dfunc"), containing 
all the components of a "dfunc" object (see \code{F.dfunc.estim}), plus, 
  \item{n.hat}{Estimated abundance in the study area (if \code{area} > 1) 
  or estimated density in the study area (if \code{area} = 1).}
  \item{ci}{The bias corrected bootstrap confidence interval for \code{n.hat}.  The names of this component 
  give the quantiles of the bootstrap distribution used to compute the bias corrected interval.}
  \item{B}{A vector or length \code{R} containing all bootstrap estimated population sizes. If a particular interation
  did not converge, the corresponding entry in \code{B} will be \code{NA}. The bootstrap distribution of 
  \code{n.hat} can be plotted with \code{hist(x$B)}, where \code{x} is an 'abundance estimate' object. The 
  confidence interval in \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)], p=names(x$ci) )}.   }
  \item{alpha}{The (scalar) confidence level of the confidence interval for \code{n.hat}.}  
  \item{n}{The number of detections (not individuals, unless all group sizes = 1) used in the estimate of abundance.}
  \item{area}{The study area size used in the estimate of abundance.}
  \item{tran.len}{The total length of transects used in the estimate of abundance.}
  \item{esw}{Effective strip width used in the estimate of abundance.  This can be computed with \code{ESW(dfunc)}.}
  \item{avg.group.size}{The average group size used in the estimate.}  
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
    
    Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}
}

\references{
Manly, B. F. J. (1997) \emph{Randomization, bootstrap, and monte carlo methods in biology}, 
London: Chapman and Hall.
}

\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
# Load the example sparrows distances and covariates datasets from package
data(sparrow.dists)
data(sparrow.covs)

# Compute perpendicular, off-transect distances from the observer's sight distance and angle
sparrow.dists$dists <- perp.dists(obs.dist=sparrow.dists$sightdist,
                                  obs.angle=sparrow.dists$sightangle)

# Fit detection function object
dfunc <- F.dfunc.estim(sparrow.dists, w.hi=150)

# Estimate abundance given a detection function
# Note, area=10000 converts to density per hectare (for distances measured in meters)
# Note, a person should do more than R=20 iterations 
fit <- F.abund.estim(dfunc, distdata=sparrow.dists, covdata=sparrow.covs, area=10000,
                     R=20, ci=0.95, plot.bs=TRUE, bs.method="transects")
}

\keyword{ model }

