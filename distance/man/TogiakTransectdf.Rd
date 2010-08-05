\name{TogiakTransectdf}
\alias{TogiakTransectdf}

\docType{data}
\title{Distance Sampling Data Objects from Brown Bear Survey at Togiak National Wildlife Refuge, Alaska}
\description{
  Example data objects from aerial line-transect surveys of brown bears (\emph{Ursus arctos}) on Togiak National Wildlife Refuge in western Alaska, USA.
  The distance sampling surveys followed the methods of Becker and Quang (2009).  The data objects are used to illustrate the gamma-shaped detection function methods (see \code{\link{FitGMRDSmodel}}).

  \code{TogiakTransectdf} contains information on each transect surveyed. 

  \code{\link{TogiakObjectdf}} contains information on each brown bear group detected.
  
  
  \code{\link{TogiakModel}} is a fitted Gamma-shaped mark-recapture distance sampling model (see \code{\link{FitGMRDSmodel}}). 
  
  \code{\link{TogiakBootSummary}} is a bootstrap summary object (see \code{\link{BootSummary}}).  
}


\usage{data(TogiakTransectdf)}
\format{
  A data frame with 969 observations (transects surveyed) on the following 5 variables:
  \describe{
    \item{\code{TransNo}}{numeric ID for each surveyed transect}
    \item{\code{YR}}{factor identifying survey year, levels \code{2003} \code{2004}}
    \item{\code{Length}}{numeric, length of surveyed transect (km)}
    \item{\code{Area}}{numeric, area surveyed along transect (\eqn{km^2})}
    \item{\code{TranElev}}{numeric, elevation of transect projection on ground - distinguishes contour transects from linear transects}
  }
}
\details{
Transects were random selected with intended length 25 km.  In relatively flat terrain, transects were linear with a hinge at the midpoint following a randomly selected angle.  In terrain with relief, transects were started at a randomly selected point then followed the contour at that elevation for the intended distance ('contour transects').

\code{TogiakTransectdf$Area} was calculated in GIS to account for the curvature of the contour transects (using an effective strip width of 750m and a blind strip offset of 22m).
}



\source{
  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
}

\seealso{\code{\link{TogiakObjectdf}}, \code{\link{FitGMRDSmodel}}, \code{\link{BootstrapGMRDSmodel}}, \code{\link{GoF.GMRDS}}} 



\keyword{datasets}
