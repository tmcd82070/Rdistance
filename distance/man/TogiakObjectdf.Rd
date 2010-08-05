\name{TogiakObjectdf}
\alias{TogiakObjectdf}


\docType{data}
\title{Distance Sampling Data Objects and Model from Brown Bear Survey at Togiak National Wildlife Refuge, Alaska}
\description{
  Example data objects from aerial line-transect surveys of brown bears (\emph{Ursus arctos}) on Togiak National Wildlife Refuge in western Alaska, USA.
  The distance sampling surveys followed the methods of Becker and Quang (2009).  The data objects are used to illustrate the gamma-shaped detection function methods (see \code{\link{FitGMRDSmodel}}).

  \code{TogiakObjectdf} contains information on each brown bear group detected.
  
  \code{\link{TogiakTransectdf}} contains information on each transect surveyed. 
  
  \code{\link{TogiakModel}} is a fitted Gamma-shaped mark-recapture distance sampling model (see \code{\link{FitGMRDSmodel}}). 
  
  \code{\link{TogiakBootSummary}} is a bootstrap summary object (see \code{\link{BootSummary}}).  
}
\usage{data(TogiakObjectdf)}
\format{
  A data frame with 177 observations (group detections) on the following 17 variables.
  \describe{
    \item{\code{GroupID}}{numeric ID for each group detection}
    \item{\code{OBS}}{numeric code identifying who detected the group: 1 - 'Pilot' only, 2 - 'Passenger' only, 3 - both}
    \item{\code{DistanceOffTransect}}{perpendicular distance from transect to detected group (m)}
    \item{\code{YR}}{factor identifying survey year, levels \code{2003} \code{2004} }
    \item{\code{TransNo}}{numeric ID for each surveyed transect}
    \item{\code{GroupSize}}{total number of animals in each detected group}
    \item{\code{GROUPTYPE}}{factor identifying \emph{group type}, levels:
         \code{AdultSub} - subadult (unknown gender),
         \code{AdultUnk} -  adult (unknown gender),
         \code{Breeding} - breeding pair (adult sow and boar),
         \code{FemaleWC} - adult sow with cubs of the year,
         \code{FemaleWY1} - adult sow with yearlings,
         \code{FemaleWY2} - adult sow with two year old or older offspring,
         \code{LrgMale} - adult boar.}
    \item{\code{PCTCOVER}}{percent canopy/shrub cover within 10 m of detected group}
    \item{\code{EFFDIST}}{\emph{effective search distance} - the maximum distance being actively scanned or searched immediately prior to detection}
    \item{\code{DAY}}{numeric, survey day (of \code{Month})}
    \item{\code{MONTH}}{numeric, month of survey}
    \item{\code{ADJDATE}}{Julian date of survey relative to origin at Jan 1 of survey year}
    \item{\code{Ncub}}{number of cubs in group}
    \item{\code{Nyrlg}}{number of yearlings in group}
    \item{\code{NSubAdlt}}{number of subadults in group}
    \item{\code{NLegal}}{number of two year old or older bears in group}
    \item{\code{AdjDateCntrd}}{Julian date of survey centered around mean survey date}
  }
}




\source{
  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
}

\seealso{\code{\link{TogiakTransectdf}}, \code{\link{TogiakModel}}, \code{\link{FitGMRDSmodel}}, \code{\link{BootstrapGMRDSmodel}}, \code{\link{GoF.GMRDS}}} 

\examples{

## plot fitted model for Pilot
library(lattice)
## Fitted Model
TrashOut<-TogiakModel

############################################################
#### Selecting Representative Covariate Combinations #######
############################################################
## log(Effective Search Distance) - quartile values of observed distribution
## each will be used on a different panel of the plot
logESD<-quantile(TrashOut$XdesPilotF[,2],probs=c(.25,.50,.75), na.rm = FALSE) 

## AdjDate -  Select three equally spaced values for depiction.
# Each value will be used in a single curve per panel.
AdjDate<-c(-3.5,0,3.5)

# y - distance of detected object from transect
# main covariate; will form x-axis of scatterplot.
trashPredictorDistance<-seq(22,750,5)

################################################################
#### Get Parameter Estimates - see Becker and Quang (2009) #####
################################################################
## shape parameter - last component of parameter vector
r1 <- TrashOut$b1[4]

## calculate b component of scale
bscale <- (1/gamma(r1))*(((r1-1)/exp(1))^(r1-1))

## covariate coefficients (and intercept)
betas <- TrashOut$b1[-4]

## create data frame
trashlength<-length(trashPredictorDistance)
trashPredict<-data.frame(Distance=rep(trashPredictorDistance,9),
        Inter=rep(1,trashlength*9),
        LogESD=rep(rep(logESD,rep(trashlength,3)),3),
        AdjDate=rep(AdjDate,rep(trashlength*3,3)))

## calculate detection densities
lambda <- exp(as.matrix(trashPredict[,-1])\%*\%betas)

blah<-trashPredict$Distance/(lambda*bscale)
trashPredict$detect<-TrashOut$hvec[4]*(1/(gamma(r1)*bscale))*
        (blah^(r1-1))*exp(-1*blah)
Apex <- bscale*(r1-1)*lambda 

#############################
# Display Pilot detection model
#############################
xyplot(detect~Distance|as.factor(LogESD),data=trashPredict,
    # 'groups' argument automatically overlays the different curves for the
    #          different 'AdjDate' values (treating AdjDate like a grouping factor)
    groups=AdjDate,type="l",lwd=2,
    layout=c(1,3),xlab="Distance from Transect",ylab="P(Detect)",
    #main="Pilot Detection Function",
    # add legend at top of plot
    key=list(space="top",
        text=list("Early"),lines=list(lty=1,col="darkgreen"),
        text=list("Mid"),lines=list(lty=2,col="red"),
        text=list("Late"),lines=list(lty=3,col="blue"),lwd=2),
    # create specific labels for each panel's strip
    strip=strip.custom(style=1, factor.levels=c("ESD = 350 m",
          "ESD = 525 m", "ESD = 758 m"), bg="white"))
}

\keyword{datasets}
