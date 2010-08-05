\name{GammaMRDS-package}
\alias{GammaMRDS-package}
\alias{GammaMRDS}
\alias{GMRDS}
\concept{distance sampling}
\concept{line-transect}
\concept{aerial surveys}
\concept{detection}

\docType{package}
\title{Functions and data sets for Fitting Gamma-shaped Detection Functions to Line-Transect Distance Sampling Data}
\description{
    Fits a detection function to line transect distance sampling data using a Gamma distribution, 
    thus allowing for unimodal rather than just monotonoically decreasing detection functions.  The Gamma's 
    scale parameter can be modeled as a function of covariates.  Data is assumed to have been collected by two observers ('Pilot', 'Passenger') sharing a common platform 
    ('mark-recapture' distance sampling).
}
\details{
\tabular{ll}{
Package: \tab GammaMRDS\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2009-07-07\cr
Depends: \tab R (>= 2.5.0) \cr
Imports: \tab trust, nlme, boot, stats \cr
Suggests: \tab lattice \cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
The package contains the following functions with
 online help available.
\tabular{ll}{
    \code{\link{FitGMRDSmodel}} \tab     Fits a gamma-shaped detection function \cr
    \code{\link{GoF.GMRDS}}          \tab Goodness-of-Fit test for fitted gamma-shaped detection function \cr
    \code{\link{BootstrapGMRDSmodel}} \tab Bootstrap fitting of a gamma-shaped detection function \cr
    \code{\link{BootSummary}} \tab Summarize statistics from object created from call to \code{boot} }
    
    
The package contains the following data objects with online help available.
\tabular{ll}{
    \code{\link{TogiakTransectdf}} \tab Transect information from brown bear survey on Togiak National Wildlife Refuge \cr
    \code{\link{TogiakObjectdf}}   \tab Detected bear groups information from survey on Togiak National Wildlife Refuge \cr
    \code{\link{TogiakModel}} \tab      Fitted gamma-shaped detection model from analysis of Togiak brown bear survey data \cr
    \code{\link{TogiakBootSummary}} \tab \code{BootSummary} object from analysis of Togiak brown bear survey data}
    

}
\author{Pham Quang, Earl Becker \email{earl.becker@alaska.gov},\cr
    Joel Reynolds \email{Joel\_Reynolds@fws.gov},\cr
    with contributions from Aaron Christ and Brook Russell

Maintainer: Joel Reynolds \email{Joel\_Reynolds@fws.gov}

}
\references{
\cite{Becker, E.F. and Quang, P. X., 2009,  A 
gamma-shaped detection function for line-transect surveys with 
mark-recapture and covariate data.  \emph{JABES} \bold{14}(2):207 -- 223.} 

\cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}

}

\keyword{package}

\examples{
## Example objects from Togiak NWR brown bear survey
## only observations within (wb, wMax)=(22m, 750m)
  Objectdf <- TogiakObjectdf 
  Transectdf <- TogiakTransectdf

## Pi1ot scale model design matrix: log of the effective search distance
## (see Becker and Quang 2009) and centered julian calendar date centered
  XdesPilot <- model.matrix(DistanceOffTransect ~ log(EFFDIST)+AdjDateCntrd, 
                            data=Objectdf)
  dimnames(XdesPilot)[[2]] <- c("Intercept","ln(EFFDIST)","AdjDateCntrd")        

## Passenger's: log(EFFDIST), and survey year as factor. 
  XdesPassenger <- model.matrix(DistanceOffTransect ~ log(EFFDIST) + I(YR==2004),
                                data=Objectdf)  
  dimnames(XdesPassenger)[[2]] <- c("Intercept","ln(EFFDIST)","YR2004")         

## Assign starting values (Intercept and shape param set to 3)
  startbPilot <-  c(3,rep(1,dim(XdesPilot)[2]-1),3) 
  startbPassenger <-  c(3,rep(1,dim(XdesPassenger)[2]-1),3)        

## Fit the detection model. 
DetectionModel <- FitGMRDSmodel(ObjectData=Objectdf,
    XdesPilot=XdesPilot,XdesPassenger=XdesPassenger,
    startbPilot=startbPilot, startbPassenger=startbPassenger,
    wMax=750, wb=22, modeltitle="Togiak Example")

## Calculate surveyed area 
  area <- sum(Transectdf$Area,na.rm=TRUE) 
## [1] 16544.42

# Estimate density of objects (e.g., brown bears) in study area 
  cat("Overall Animal Density = ", 1000*DetectionModel$Na/area," per 1000 km^2\n") 
## Overall Animal Density =  40.35072  per 1000 km^2

## using Togiak NWR example data, but just 100 replicates to illustrate
## USE MORE REPLICATES IN REAL LIFE
 boot.out <- BootstrapGMRDSmodel(Transectdf, Objectdf, 
    FittedModel=DetectionModel,Strata=Transectdf$YR,    
    UseCubFix=FALSE, R=100)

   ExampleBootSummary<-BootSummary(bootObject=boot.out, CItype=c('perc'),CONF=0.95)

   ExampleBootSummary

## Param.     Est     Bias   RelBias  StdError  CV   LowerPerc'tile  UpperPerc'tile 
##   Ng    378.1178   4.5536  0.0120  44.3266  0.1172  301.2090        472.7558 
##   Na    666.2586   8.5613  0.0128  85.2152  0.1279  524.2119        848.4671
##    ...                          

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
betas <- as.matrix(TrashOut$b1[-4],ncol=1)

## create data frame
trashlength <- length(trashPredictorDistance)
trashPredict <- data.frame(Distance=rep(trashPredictorDistance,9),
        Inter=rep(1,trashlength*9),
        LogESD=rep(rep(logESD,rep(trashlength,3)),3),
        AdjDate=rep(AdjDate,rep(trashlength*3,3)))

## calculate detection densities
  lambda <- exp(as.matrix(trashPredict[,-1])\%*\%betas)
  blah <- trashPredict$Distance/(lambda*bscale)
  trashPredict$detect <- TrashOut$hvec[4]*(1/(gamma(r1)*bscale))*
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
