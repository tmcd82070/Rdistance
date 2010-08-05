`FitGMRDSmodel` <-
function(ObjectData, XdesPilot, XdesPassenger, wMax,
    wb=0, startbPilot, startbPassenger, modeltitle=NULL,UseCubFix=FALSE)
{
# Calls function .GMRDSmodel to fit detection function model for double
# observer line transect survey data.
# Called from MainScriptModelFit.R; calls .GMRDSmodel.
####### ARGUMENTS #######
# ObjectData:   matrix containing the following named columns (at minimum):
#               column1= $GroupID - Bear group id,
#               column2= $OBS - observer code(1-Pilot only, 2-Passenger only, 3-both), 
#               column3= $DistanceOffTransect - distance from the transect to the bear, 
#               and $GroupSize - number of detected bears in group
#                   $NLegal - number of 'Legal' aged bears in group
#                   $Ncub - number of detected cubs in group
#                   $Nyrlg - number of detected yearlings in group
# XdesPilot:    The Pilot's DESIGN MATRIX
#               column1=vector of ones denoting the intercept term
#               column2 ... - rest of design matrix for the Pilots detection model
# XdesPassenger: The Passenger's DESIGN MATRIX
#               column1=vector of ones denoting the intercept term
#               column2 ... - rest of design matrix for the Passengers detection model
# wMax         maximum sighting distance allowed by model 
#               - set by modeler - usually ~95th P72ercentile
# wb            edge of blind spot - use 22 m for Supercun 100 m. agl
# startPilot:     initial guesses for Pilot's Line-Tran. model Coefficients
# startPassenger:  initial guesses for Passenger's Line-Tran. model Coefficients
# modeltitle:     Title for model run
# UseCubFix       if T then correct estimator so that # cub Groups >= # Yearling Groups - Rec. for Brown Bears
#####################
# Author: Joel Reynolds 10/08 
#################### 
# Last Edit 28 May 2009    
###################

# NOTE:    
# USE.PGAMMA.WB   tells whether to use second pgamma in likelihood (see lt6.lik)
#                      defaults to false (recommend F)
# USE.INT.H       if T then use .GammaLnTrns.h.Int (integration); 
#                 if F then use GammaLnTrns.h.noInt (recommended - more stable and faster)

## if not already installed, add library(nlme)
  #Available<-require("nlme", quietly=TRUE, character.only=TRUE)
  #if (!Available) stop("Requires installation of R package 'nlme'.\nPlease download from CRAN via the 'Packages =>Install Package' menu.")  
## if not already installed, add library(trust)
  #Available<-require("trust", quietly=TRUE, character.only=TRUE)
  #if (!Available) stop("Requires installation of R package 'trust'.\nPlease download from CRAN via the 'Packages =>Install Package' menu.")  
  
  if (is.null(ObjectData$Ncub)) ObjectData$Ncub <- 0 
  if (is.null(ObjectData$Nyrlg)) ObjectData$Nyrlg <- 0
    
return( .GMRDSmodel(ObsYdat=cbind(ObjectData$GroupID,ObjectData$OBS,ObjectData$DistanceOffTransect), XdesPilot=XdesPilot,          
    XdesPassenger=XdesPassenger,wMax=wMax, wb=wb,
    startbPilot=startbPilot, startbPassenger=startbPassenger,
    modeltitle=modeltitle, size=ObjectData$GroupSize, LegalSize=ObjectData$NLegal,
    BootRun=FALSE, USE.PGAMMA.WB=FALSE, USE.INT.H=FALSE,          
    UseCubFix=UseCubFix,CubVec=ifelse(ObjectData$Ncub == 0,0,1),YrlgVec=ifelse(ObjectData$Nyrlg==0,0,1)))
}
