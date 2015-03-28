\name{Rdistance-package}
\alias{Rdistance-package}
\alias{distance}
\alias{Rdistance}

\docType{package}
\title{
Rdistance - Distance Sampling Analyses for Line Transect Data
}
\description{
\bold{Description} - 
This package contains functions and associated routines to analyze line transect data, and functionality for point count data is forthcoming.  A large suite of classical, parametric detection functions are included (see \code{\link{F.dfunc.estim}}), and the package can also accommodate user-defined detection functions.  

\bold{Background} -
Line transect surveys are conducted by traversing \emph{randomly} 
placed transects in a study area with the objective of estimating 
density or abundance of a particular organism.  Data collected during line 
transect surveys consists of sighting records for \emph{targets}, usually either individuals or 
groups of some species.  Among the collected data, 
off-transect distances are recorded or computed from other information such as sighting distance and angle (see \code{\link{perp.dists}}).  Off-transect distances are 
the perpendicular distances from the transect to the location of the initial 
sighting cue.  The physical locations of sighted targets are often recorded or computed. When groups are the target, the number of individuals in the group is recorded. 

A fundamental characteristic of distance sampling analyses is that sightability (probability of 
detection) of targets is assumed to decline as off-transect distances increase.
Targets far from the transect are assumed to be harder to detect than targets close to the transect.  In most classical line transect studies, targets on the transect (off-transect distance 
= 0) are assume to be sighted with 100\% probability.  This assumption 
allows estimation of the proportion of targets missed during the 
survey, and thus adjust the actual number of sighted targets by this proportion.
Some studies utilize two observers searching the same areas to estimate the 
proportion of indivivduals missed and thereby eliminating the assumption 
that all individuals on the line have been observed. 


\bold{Relationship to DISTANCE} - 
Program DISTANCE (www.distancesampling.org) is a stand-alone 
program for analyzing line transect 
data and has, with a few exceptions, been the standard program for analyzing 
line transect data. For the most part, \code{Rdistance} performs a subset 
of the analyses available in 
program DISTANCE.  In particualar, routines in \code{Rdistance} 
perform the same "classical distance analysis" (CDA) as DISTANCE, with 
a couple differences.
\code{Rdistance} includes routines to fit a Gamma detection function, and 
the Uniform detection function of Rdistance is parameterized as a heavy-side function. 

The best place to start learning the routines of \code{Rdistance} is the 
examples in the help files for \code{F.dfunc.estim}, \code{F.abund.estim}, and \code{F.automated.CDA}.
}

\details{
\tabular{ll}{
Package: \tab Rdistance\cr
Type: \tab Package\cr
License: \tab GNU General Public License\cr
}

A list of routines can be obtained by loading \code{Rdistance} and 
issuing \code{ls(pos="package:Rdistance")}.
}

\author{
Main authtor and maintainer: Trent McDonald <tmcdonald@west-inc.com>

Coauthors:  Ryan Nielson and Jason Carlisle

Contributors: James Griswold, Joel Reynolds, Pham Quang, Earl Becker, Aaron Christ, Brook Russelland, and Patrick McKann
}


\keyword{ package }