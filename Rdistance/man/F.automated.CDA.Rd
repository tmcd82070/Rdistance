\name{F.automated.CDA}
\alias{F.automated.CDA}

\title{
F.automated.CDA - Automated classical distance analysis.
}

\description{
Perform automated classical distance function selection and estimation of abundance. 
}

\usage{
F.automated.CDA(dist, group.size = 1, area = 1, total.trans.len = 1, w.lo = 0, w.hi = max(dist), 
    likelihoods = c("halfnorm", "hazrate", "uniform", "negexp", "Gamma"), 
    series = c("cosine", "hermite", "simple"), expansions = 0:3, plot = TRUE, ...)
}

\arguments{
  \item{dist}{
    The vector of observed off-transect distances. All values in \code{dist} must be 
    between \code{w.lo} and \code{w.hi}. 
}

  \item{group.size}{
    Vector of group sizes for every element in \code{dist}.  This vector is replicated 
    as necessary to be the same length as \code{dist}.  Default value 
    is 1, which assumes all sightings were individuals.
}

  \item{area}{
    Size of the study area, in squared units of distance measures (e.g., if units of off-transect distances 
    is meters, study area size should be in square meters).  Default value of 1 produces density estimates.  
    See documentation for \code{F.abund.estim} for more description of this parameter.
}

  \item{total.trans.len}{
    Total length of all transects in the study area, in units of the distance measures (e.g., if units of 
    off-transect distances is meters, total transect length should be meters).  Default value is 1.
}

  \item{w.lo}{
    Lower or left-truncation limit of the distances in \code{dist}.  This is the minimum 
    possible off-transect distance. Default is 0.
}

  \item{w.hi}{
    Upper or right-truncation limit of the distances in \code{dist}. This is the maximum 
    off-transect distance that could be observed. Default is the maximum observed
    distance.
}

  \item{likelihoods}{
    Vector of strings specifying the likelihoods to consider during model selection. Valid values at present are "uniform", 
    "halfnorm", "hazrate", "negexp", and "Gamma". See Details for the models this routine considers. 
}

  \item{series}{
    Vector of series types to consider during model selection.     
    Valid values are 'simple', 'hermite', and 'cosine'. See Details for the models this routine considers.
}

  \item{expansions}{
    Vector of the number of expansion terms to consider during model selection. 
    Valid values are 0 through 3. See Details for the models this routine considers.
}

  \item{plot}{
    Logical scalar specifying whether to plot models during model selection.  If TRUE, a histogram with 
    fitted distance function is plotted for every fitted model.  The function pauses between each plot 
    and prompts the user for whether they want to continue or not. For completely automated estimation, set 
    \code{plot} = FALSE. 
}

  \item{...}{
    Additional parameters passed to \code{F.dfunc.estim}, which in turn are passed to \code{F.gx.estim}.  
    These include \code{x.scl}, \code{g.x.scl}, and \code{observer} for estimating double observer probabilities. 
  }
}

\details{
    During model selection, each series and number of expansions is 
    crossed with each of the likelihoods. For example, if \code{likelihoods} has 3 elements, 
    \code{series} has 2 elements, and \code{expansions} has 4 elements, the total number of models fitted is 
    3 (likelihoods) X 2 (series) X 4 (expansions) = 24 models.  By default, every available (classic) distance 
    function is fitted.  The default specification results in 5 (likelihoods) X 3 (series) X 4 (expansions) = 60 
    fitted models.  The model with lowest AIC is choosen as 'best', and estimation of abundance proceeds 
    using that model.
    
}   
 
\value{
   An 'abundance estimate' object (see \code{F.abund.estim} and \code{F.dfunc.estim}). 
   Returned abundance estimates are based on the best fitting distance function amoung those
   fitted.  
   
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{F.abund.estim}}
}

\examples{

\dontrun{
set.seed(234252)
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]

#   Assume surveyed 10000 m in study area of size 1000 hectares.
N <- F.automated.CDA( x, area=1000*10000, total.trans.len=1000, w.hi = 100, 
    likelihood=c("halfnorm", "uniform", "Gamma"), series=c("cosine", "simple"), 
    expansions=c(0,1,2))
}
}

\keyword{ model }

