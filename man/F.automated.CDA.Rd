\name{F.automated.CDA}
\alias{F.automated.CDA}

\title{
Automated classical distance analysis.
}

\description{
Perform automated classical detection function selection and estimation of abundance. 
}

\usage{
F.automated.CDA(detection.data, transect.data, w.lo=0, w.hi=max(dist),
likelihoods=c("halfnorm", "hazrate", "uniform", "negexp", "Gamma"),
series=c("cosine", "hermite", "simple"), expansions=0:3, warn=TRUE,
area=1, ci=0.95, R=500, by.id=FALSE, plot.bs=FALSE, plot=TRUE, ...)

}

\arguments{
\item{detection.data}{
This parameter is passed to \code{F.dfunc.estim} and \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{transect.data}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{w.lo}{
This parameter is passed to \code{F.dfunc.estim}.
See \code{F.dfunc.estim} documentation for definition.}

\item{w.hi}{
This parameter is passed to \code{F.dfunc.estim}.
See \code{F.dfunc.estim} documentation for definition.}

\item{warn}{
This parameter is passed to \code{F.dfunc.estim}.
See \code{F.dfunc.estim} documentation for definition.}

\item{area}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{ci}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{R}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{by.id}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{plot.bs}{
This parameter is passed to \code{F.abund.estim}.
See \code{F.abund.estim} documentation for definition.}

\item{likelihoods}{
Vector of strings specifying the likelihoods to consider during model selection. Valid values at present are "uniform", "halfnorm", "hazrate", "negexp", and "Gamma". See Details for the models this routine considers.}

\item{series}{
Vector of series types to consider during model selection.     
Valid values are 'simple', 'hermite', and 'cosine'. See Details for the models this routine considers.}

\item{expansions}{
Vector of the number of expansion terms to consider during model selection. 
Valid values are 0 through 3. See Details for the models this routine considers.}

\item{plot}{
Logical scalar specifying whether to plot models during model selection.
If \code{TRUE}, a histogram with fitted distance function is plotted for every fitted model.  The function pauses between each plot and prompts the user for whether they want to continue or not. For completely automated estimation, set \code{plot} = \code{FALSE}.}

\item{...}{
Additional parameters passed to \code{F.dfunc.estim}, which in turn are passed to \code{F.gx.estim}. These include \code{x.scl}, \code{g.x.scl}, and \code{observer} for estimating double observer probabilities.}
}

\details{
During model selection, each series and number of expansions is crossed with each of the likelihoods. For example, if \code{likelihoods} has 3 elements, \code{series} has 2 elements, and \code{expansions} has 4 elements, the total number of models fitted is 3 (likelihoods) * 2 (series) * 4 (expansions) = 24 models.  The default specification fits 41 detection functions from the "halfnorm", "hazrate", "uniform", "negexp", and "Gamma" likelihoods (note that Gamma does not currently implement expansions, see \code{\link{Gamma.like}}).  The model with lowest AIC is choosen as 'best', and estimation of abundance proceeds using that model.
}   
 
\value{
   An 'abundance estimate' object (see \code{F.abund.estim} and \code{F.dfunc.estim}). 
   Returned abundance estimates are based on the best fitting distance function among those fitted.  
   
}

\author{
    Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
    
    Jason Carlisle, WEST Inc., \email{jcarlisle@west-inc.com}
}


\seealso{
\code{\link{F.dfunc.estim}}, \code{\link{F.abund.estim}}
}

\examples{
# Load the example datasets of sparrow detections and transects from package
data(sparrow.detections)
data(sparrow.transects)

# Automate fitting multiple detection functions
# And estimate abundance (density per ha in this case) given the 'best' detection function
# Note, area=10000 converts to density per hectare (for distances measured in meters)
# Note, a person should do more than R=20 iterations 
F.automated.CDA(detection.data=sparrow.detections, transect.data=sparrow.transects,
                likelihood=c("halfnorm", "hazrate", "negexp"),
                series=c("cosine", "simple"),
                expansions=c(0, 1), area=10000, R=20, ci=0.95, by.id=FALSE,
                plot.bs=FALSE, w.hi=150, plot=TRUE)
}

\keyword{ model }

