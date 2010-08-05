\name{TogiakBootSummary}
\alias{TogiakBootSummary}
\docType{data}
\title{A Bootstrap Summary Object}
\description{
  Example Bootstrap summary object from bootstrap analysis of fitting a gamma-shaped detection model to the brown bear aerial survey data from Togiak National Wildlife Refuge. 
}


\source{
  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
}
\seealso{\code{\link{TogiakTransectdf}}, \code{\link{TogiakObjectdf}}, \code{\link{BootSummary}}, \code{\link{BootstrapGMRDSmodel}}} 

\examples{
data(TogiakBootSummary)
## plot of relative bias of various model parameters
library(lattice)

dotplot(ordered(Parameter,levels=rev(Parameter))~RelativeBias, 
        data=TogiakBootSummary, xlab="Relative Bias (0.05 = 5\%)",
        main="Togiak Brown Bear Detection model Parameters")
}
\keyword{datasets}
