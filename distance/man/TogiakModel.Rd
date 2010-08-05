\name{TogiakModel}
\alias{TogiakModel}
\docType{data}
\title{A Fitted Gamma-shaped Detection Model}
\description{
Gamma-shaped mark-recapture distance sampling  ('GammaMRDS') detection model fit to the brown bear aerial survey data from Togiak National Wildlife Refuge. 
}

\details{
See \code{\link{FitGMRDSmodel}} for description of components.
}

\seealso{\code{\link{TogiakObjectdf}}, \code{\link{TogiakTransectdf}}, \code{\link{FitGMRDSmodel}}, \code{\link{GoF.GMRDS}}} 

\source{ 
  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
}
\references{
  \cite{Becker, E.F. and Quang, P. X., 2009,  A 
gamma-shaped detection function for line-transect surveys with 
mark-recapture and covariate data.  \emph{JABES} \bold{14}(2):207 -- 223.}
}
\examples{
data(TogiakModel)


## plot fitted model for Pilot
library(lattice)
## Fitted Model
TrashOut <- TogiakModel

############################################################
#### Selecting Representative Covariate Combinations #######
############################################################
## log(Effective Search Distance) - quartile values of observed distribution
## each will be used on a different panel of the plot
logESD <- quantile(TrashOut$XdesPilotF[,2],probs=c(.25,.50,.75), na.rm = FALSE) 

## AdjDate -  Select three equally spaced values for depiction.
# Each value will be used in a single curve per panel.
AdjDate <- c(-3.5,0,3.5)

# y - distance of detected object from transect
# main covariate; will form x-axis of scatterplot.
trashPredictorDistance <- seq(22,750,5)

################################################################
#### Get Parameter Estimates - see Becker and Quang (2009) #####
################################################################
## shape parameter - last component of parameter vector
r1 <- TrashOut$b1[4]

## calculate b component of scale
bscale <- (1/gamma(r1))*(((r1-1)/exp(1))^(r1-1))

## covariate coefficients (and intercept)
betas <- matrix(TrashOut$b1[-4],ncol=1)

## create data frame
trashlength <- length(trashPredictorDistance)
trashPredict <- data.frame(Distance=rep(trashPredictorDistance,9),
        Inter=rep(1,trashlength*9),
        LogESD=rep(rep(logESD,rep(trashlength,3)),3),
        AdjDate=rep(AdjDate,rep(trashlength*3,3)))

## calculate detection densities
lambda <- exp(as.matrix(trashPredict[,-1])\%*\%betas)

blah <- trashPredict$Distance/(lambda*bscale)
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
