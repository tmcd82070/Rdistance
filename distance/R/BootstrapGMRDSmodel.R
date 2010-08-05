`BootstrapGMRDSmodel` <-
function(TransectData, ObjectData, FittedModel, Strata=rep(1,length(TransectData[,1])), UseCubFix=FALSE, R=2000,...)
{
# bootstrap detection function model for double-observer 
# line transect survey data.
# Called from MainScriptModelFit.R; calls boot, which calls .GMRDSmodelboot.
####### ARGUMENTS #######
# TransectData:   AT LEAST 3 column data frame of ALL transect data as follows:
#                    TransNo, Length,Area
# ObjectData:   matrix containing the following named columns (at minimum):
#               column1= $GroupID - Bear group id,
#               column2= $OBS - observer code(1-Pilot only, 2-Passenger only, 3-both), 
#               column3= $DistanceOffTransect - distance from the transect to the bear, 
#               and $GroupSize - number of detected bears in group
#                   $NLegal - number of 'Legal' aged bears in group
#                   $Ncub - number of detected cubs in group
#                   $Nyrlg - number of detected yearlings in group 
# FittedModel:  Detection model created from a call to FitGMRDSmodel()
# Strata        vector identifying strata levels, for passing to boot()
#               so that bootstrap resampling occurs w/in strata.
# UseCubFix       if T then correct estimator so that # cub Groups >= # Yearling Groups - Rec. for Brown Bears
# R             number of bootstrap replicates
# ...           other arguments for passing to boot()
#####################
# Author: Joel Reynolds 29 May 09
#################### 
# Last Edit 4 June 2009    
###################

 ## if not already installed, add library(boot)
  #bootAvailable<-require("boot", quietly=TRUE, character.only=TRUE)
  #if (!bootAvailable) stop("Requires installation of R package 'boot'.\nPlease download from CRAN via the 'Packages =>Install Package' menu.")  
  
  if (missing(TransectData)) stop("Needs TransectData data frame.")
  if (missing(ObjectData)) stop("Needs ObjectData data frame.")
  if (missing(FittedModel)) stop("Needs fitted model.")
      
  #get number of params in Pilot model
  nPilotpar<-dim(FittedModel$XdesPilotF)[2]
  #get number of params in Passenger model
  nPassengerpar<-dim(FittedModel$XdesPassengerF)[2]
  
  if (is.null(ObjectData$Ncub)) ObjectData$Ncub <- 0 
  if (is.null(ObjectData$Nyrlg)) ObjectData$Nyrlg <- 0

return( boot(TransectData, statistic=.GMRDSmodelboot, R=R,strata=Strata,
    ObsYdat=cbind(ObjectData$GroupID,ObjectData$OBS,ObjectData$DistanceOffTransect,ObjectData$GroupSize,ObjectData$NLegal), 
    XdesPilot=FittedModel$XdesPilotF[,-nPilotpar], 
    XdesPassenger=FittedModel$XdesPassenger[,-nPassengerpar], 
    wMax=FittedModel$wMax, wb=FittedModel$wb, 
    startbPilot=FittedModel$b1, startbPassenger=FittedModel$b2,  
    BearTran=ObjectData$TransNo,UseCubFix=FALSE, 
    CubVec=ifelse(ObjectData$Ncub == 0,0,1),YrlgVec=ifelse(ObjectData$Nyrlg==0,0,1),...))
    
}
