\name{BootstrapGMRDSmodel} 
\alias{BootstrapGMRDSmodel} 
\concept{distance sampling} 
\concept{bootstrap} 
\concept{detection}

 \title{Bootstrap the fitting of a Gamma-shaped Detection Function} 
 
 \description{A wrapper for using the \code{boot} function to bootstrap the fitting of a gamma-shaped detection function to line transect distance sampling data.} 

\usage{ 
BootstrapGMRDSmodel(TransectData, ObjectData, FittedModel, 
                    Strata = rep(1,length(TransectData[,1])), 
                    UseCubFix = FALSE, R = 2000,...) }

\arguments{
  \item{TransectData}{Data frame describing each surveyed transect.  Must contain the following named columns (at minimum):
                TransNo - unique id for the transect;
                Length - surveyed length;
                Area - total area surveyed along transect.}
  \item{ObjectData}{Data frame describing each detected group.  Must contain the following named columns (at minimum): 
                    \tabular{lll}{
                     \tab GroupID  \tab unique id for the detected group \cr
                     \tab OBS     \tab who detected the group? Coded as 1-Pilot only, 2-Passenger only, 3-both \cr
                     \tab DistanceOffTransect \tab perpendicular distance from the transect to the group \cr
                     \tab TransNo \tab unique id for the transect \cr
                     \tab GroupSize \tab number of animals detected in the group \cr
                     \tab NLegal  \tab number of 'Legal' aged bears (\eqn{ \ge 2}) in group (can be vector of 0s) \cr
                     \tab Ncub    \tab number of cubs in group (can be vector of 0s) \cr
                     \tab Nyrlg   \tab number of yearlings in group (can be vector of 0s) }}
  \item{FittedModel}{Object returned by call to \code{\link{FitGMRDSmodel}}}
  \item{Strata}{Optional vector identifying strata membership of each transect; for use when bootstrap resampling. Defaults to one strata.}  
  \item{UseCubFix}{Logical.  Optional correction factor for bear surveys to adjust for undercounting groups with cubs.  See \code{\link{FitGMRDSmodel}}.}
  \item{R}{Total number of bootstrap resamples desired. Recommend default, R = 2000.}
  \item{...}{Other arguments to pass to the function \code{\link[boot]{boot}}.}
} 

\details{
\code{BootstrapGMRDSmodel} is an interface to the \code{\link[boot]{boot}} function specifically for 
nonparametric bootstrap resampling and fitting a gamma-shaped detection 
model to distance sampling data.}



\value{
\code{BootstrapGMRDSmodel} passes the LIST returned by 
\code{\link[boot]{boot}}.  The matrix \code{t} in this list has 
the following columns for each bootstrap replicate:\cr
    \item{Ng}{estimated total number of objects (groups) in the surveyed area}
    \item{Na}{estimated total number of animals in the surveyed area}
    \item{NaLegal}{estimated total number of 'Legal-sized' animals in the surveyed area}
    \item{NgDens}{estimated density of groups in the study region}
    \item{NaDens}{estimated density of animals in the study region}
    \item{NaLegDens}{estimated density of 'Legal' aged animals in the study region}
    \item{PilotMaxDetect}{Pilot's estimated maximum probability of detection}
    \item{PassengerMaxDetect}{Passenger's estimated maximum probability of detection}
    \item{Pilot Covariates}{Pilot's detection function parameter estimates (scale model coefficients, shape parameter)}
    \item{Passenger Covariates}{Passenger's detection function parameter estimates (scale model coefficients, shape parameter)}
    \item{n1}{number of objects detected only by Pilot}    
    \item{n2}{number detected only by Passenger}
    \item{n3}{number detected by both}
    \item{Area}{total area surveyed}
      
} 

\references{ \cite{Becker, E.F. and Quang, P. X., 2009,  A 
gamma-shaped detection function for line-transect surveys with 
mark-recapture and covariate data.  \emph{JABES} \bold{14}(2):207 -- 223.} 

  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
 }
\author{Joel H. Reynolds \email{Joel\_Reynolds@fws.gov}}


\section{Warning}{This can take a while to run. \cr Replicates that fail to converge will have their relevant elements set to \code{NA}s in the matrix \code{t}. }

 
\seealso{\code{\link{FitGMRDSmodel}}, \code{\link{BootSummary}}, \code{\link[boot]{boot}}} 

\examples{ 

## using Togiak NWR example data, but just 100 replicates to illustrate
## USE MORE REPLICATES IN REAL LIFE
 boot.out <- BootstrapGMRDSmodel(TogiakTransectdf, TogiakObjectdf, 
    FittedModel=TogiakModel,Strata=TogiakTransectdf$YR,    
    UseCubFix=FALSE, R=100)

 ExampleBootSummary<-BootSummary(bootObject=boot.out, CItype=c('perc'),CONF=0.95)

 ExampleBootSummary

## Param.     Est     Bias   RelBias  StdError  CV   LowerPerc'tile  UpperPerc'tile 
##   Ng    378.1178   4.5536  0.0120  44.3266  0.1172  301.2090        472.7558 
##   Na    666.2586   8.5613  0.0128  85.2152  0.1279  524.2119        848.4671
##    ...                      
} 

\keyword{models}
\keyword{multivariate}

\keyword{nonparametric}

\keyword{survey}% __ONLY ONE__ keyword per line
