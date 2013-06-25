\name{F.abund.estim}
\alias{F.abund.estim}
\title{
F.abund.estim - Estimate abundance from distance data.
}

\description{
Estimate abundance given an estimated distance function, group sizes, 
and total length of transect.
}

\usage{
F.abund.estim(dfunc, avg.group.size = 1, group.sizes, area = 1, 
    tot.trans.len = 1, n = length(dfunc$dist), ci=0.95, R=500, plot.bs=FALSE, 
    transects=NULL)
}

\arguments{
  \item{dfunc}{
    An estimated 'dfunc' object produced by \code{F.dfunc.estim}. 
}
  \item{avg.group.size}{
    Average group size for all sighted objects, estimated from some 
    other source. Either \code{avg.group.size} or \code{group.sizes} 
    must be specified. Specifying the vector of \code{group.sizes}
    takes precedent over \code{avg.group.size}.
}
  \item{group.sizes}{
    Vector of group sizes associated with each sighted object.  Takes
    precedence over \code{avg.group.size}
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
  \item{tot.trans.len}{
    Total length of transects surveyed.  Units must be same as those 
    of raw distance measures, and the square root of units of \code{area}.
}
  \item{n}{
    Number of groups seen. Normally, this is length(dist), but this is 
    left as a parameter so that a previously estimated 
    distance function can be applied to another set of surveys.  For example, 
    if \code{dfunc} is estimated and assumed to apply to a new 
    set of transects, it is possible to estimate abundance using 
    average group size in the new area and number of sighted groups. 
    }    
    
  \item{ci}{A scaler indicating the confidence level of confidence intervals.  
    Confidence intervals are computed using the bias corrected bootstrap method.  
    If \code{ci} = NULL, confidence intervals are not computed. 
    }
  
  \item{R}{The number of bootstrap iterations to conduct when \code{ci} is not NULL. 
    }
    
  \item{plot.bs}{A logical scalar indicating whether to plot individual bootstrap iterations}
  
  \item{transects}{A vector containing the transect ID's of the targets sighted.  This vector is used during bootstrapping to resample transects 
    in order to estimate variance in estimated size. Length must be either 0 (NULL) or \code{length(dfunc$dist)}.
    This is only ssed when \code{ci} is not NULL.  If \code{sighting.transects} is not NULL, transects are resampled and new 
    distance functions are fitted during bootstrap iterations.  If \code{sighting.transects} is NULL, individual observations are resampled during bootstrap iterations.  
    For the latter case (NULL), sightings must be independent and the fundamental unit of replication. 
    }   



}

\details{
The abundance estimate is 
\deqn{N = \frac{n.indiv*area}{2*ESW*tot.trans.len}}{N = n.indiv*area / (2*ESW*tot.trans.len)}
where n.indiv is either \code{avg.group.size * n} or \code{sum(group.sizes)}, and \code{ESW} 
is the effective strip width computed from the estimated distance function (i.e., \code{ESW(dfunc)}). 

The confidence interval for abundance assumes that the fundamental units of replication 
(transects or individual sightings) are independent. The 
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
  \item{n}{The number of targets used in the estimate of abundance.  Usually this is \code{length(dist)}, but 
    when supplied as an argument, it can be different.}
  \item{area}{The study area size used in the estimate of abundance.}
  \item{tran.len}{The total length of transects used in the estimate of abundance.}
  \item{esw}{Effective strip width used in the estimate of abundance.  This can be computed with \code{ESW(dfunc)}.}
  \item{avg.group.size}{The average group size used in the estimate.}  
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}

\references{
Manly, B. F. J. (1997) \emph{Randomization, bootstrap, and monte carlo methods in biology}, 
London: Chapman and Hall.
}

\seealso{
\code{\link{F.dfunc.estim}}
}

\examples{
\dontrun{
#   Fake offtransect distances (x), transect id's (tran.id), and group sizes (grp.size)
set.seed(234092)
x <- rnorm(1000) * 100
tran.id <- rep(1:100, each=10)
grp.size <- rpois(1000, 2.5)
tran.id <- tran.id[ 0 < x & x < 100 ]
grp.size <- grp.size[ 0 < x & x < 100 ]
x <- x[ 0 < x & x < 100 ]

#   Fit a distance function
hn.dfunc <- F.dfunc.estim( x, likelihood="halfnorm", w.hi = 100)

#   Assume surveyed 10000 m in study area of size 1000 hectares.
#   Original offtransect distances must be in meters, and area must be square meters. 
#   Without group sizes, this estimates number of groups in the area. 
F.abund.estim( hn.dfunc, tot.trans.len=10000, area=1000*10000, ci=NULL)

#   Include group sizes to estimate number of individuals. 
F.abund.estim( hn.dfunc, group.sizes = grp.size, tot.trans.len=10000, area=1000*10000, ci=NULL)

#   Or,
F.abund.estim( hn.dfunc, avg.group.size=2.5, tot.trans.len=10000, area=1000*10000, ci=NULL)

#   Bootstrap resample the transects to compute CI for N (A person should do more than R=100 iterations)
F.abund.estim( hn.dfunc, group.sizes=grp.size, tot.trans.len=10000, area=1000*10000, ci=.95, transects=tran.id, R=100, plot.bs=TRUE)
}
}
\keyword{ model }

