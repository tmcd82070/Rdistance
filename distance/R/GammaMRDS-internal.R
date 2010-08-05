`.GMRDSmodel` <-
function(ObsYdat, XdesPilot, XdesPassenger, wMax, wb,
    startbPilot, startbPassenger, modeltitle, size, LegalSize, 
    BootRun=FALSE, USE.PGAMMA.WB=FALSE, USE.INT.H=FALSE, 
    UseCubFix=FALSE, CubVec, YrlgVec)
{
# Main function for estimating double observer line transect detection functions.
# Called from FitGMRDSmodel; calls .b, and .hInt or .hNoInt as requested.
####### ARGUMENTS #######
# ObsYdat:      3 column matrix containing :column1=Bear id,
#               column2=obsr(1-Pilot only, 2-Passenger only, 3-both), 
#               column3= Y (distance from the transect to the bear), 
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
# size            vector of data containing group size 
# BootRun        used to control output when .GMRDSmodelboot calls .GMRDSmodel
#           if BootRun=Yes, then supress the print results
# USE.PGAMMA.WB   tells whether to use second pgamma in likelihood (see lt7.lik)
#                      defaults to false (recommend F)
# USE.INT.H       if T then use .hInt (integration); if F then use GammaLnTrns.h.noInt (recommended - more stable and faster)
# UseCubFix       if T then correct estimator so that # cub Groups >= # Yearling Groups - Rec. for Brown Bears
# CubVec          vector of 0,1's denoting Sow with cubs
# YrlgVec        vector of 0,1's denoting Sow with yearlings
#####################
# Author: Earl Becker and Pham Quong, (dates)
# Ported to R: Earl Becker and Aaron Christ 9/19/04
# Additional documentation, efficiency modifications: Joel Reynolds 11/05 - 5/09 
####################
# References:
#
#
###################
# Last Edit 4 June 2009
###################
# Version 7 updates R version LT6 (Becker and Christ) by adding Cub Correction Becker 7/6/2006
    
    # create vector to hold detection probabilities for each object (for use in H-T).
    pii <- NULL
    
    #maximum observable distance off transect
    #wMax <<- wMax
    #minimum observable distance off transect
    #wb <<- wb

    #################
    # Fit glms for scale parameter
    #################
    #fit Pilot detection model
    fit1 <- .b(Ydat = ObsYdat, Xdes = XdesPilot,
             obs = 1, start = startbPilot, wMax=wMax, wb=wb,USE.PGAMMA.WB=USE.PGAMMA.WB)
    
    #Check convergence
    if(fit1$converged==FALSE) {
        print("Failure to converge while fitting Pilot model. \n Try different starting values.")
        ConvPilot<-FALSE
        b1 <- rep(NA,dim(XdesPilot)[2]+1)
        } #end failed convergence.    
    else {
         ConvPilot <- TRUE
         b1 <- as.vector(fit1$argument)   #Pilot's parameter estimate vector
        }

    #fit Passenger detection model
    fit2 <- .b(Ydat = ObsYdat, Xdes = XdesPassenger,
             obs = 2, start = startbPassenger, wMax=wMax, wb=wb,USE.PGAMMA.WB=USE.PGAMMA.WB)

    #Check convergence
    if(fit2$converged==FALSE) {
        print("Failure to converge while fitting Passenger model. \n Try different starting values.")
        ConvPassenger <- FALSE
        b2 <- rep(NA,dim(XdesPassenger)[2]+1)
        } #end failed convergence.    
    else {
          ConvPassenger <- TRUE
          b2 <- as.vector(fit2$argument)   #Passenger's parameter estimate vector
        }
        
    ########    End of Maximum Likelihood Section ########

    #calculate no. groups observed only by Pilot, only by Passenger, by both
    n1 <- sum(ObsYdat[,2]  == 1)
    n2 <- sum(ObsYdat[,2]  == 2)
    n3 <- sum(ObsYdat[,2] == 3)
    #calculate number of groups seen by each
    nPilot <- n1 + n3
    nobs <- n2 + n3
    
    if (!ConvPilot | !ConvPassenger) { # Failure to Converge
         output <- list(Ydata = cbind(ObsYdat,GroupSize=size,NLegal=LegalSize), 
            XdesPilotF = cbind(XdesPilot, NA), XdesPassengerF = cbind(XdesPassenger,NA),
            hvec = c(n1, n2, n3, NA, NA), Ng = NA, 
            Na = NA, seNgHT=NA, seNaHT=NA, NaLegal = NA,
            CubCF =NA,Ng_adj =NA ,Na_adj =NA, b1=b1,b2=b2,
            ### DROPPED in favor of bootstrap, Joel Reynolds 3 Oct 2008
            # SDE1 = SDE1, SDE2 = SDE2, ttest1 = ttest1, ttest2 = ttest2,
            wMax = wMax, wb = wb, modeltitle = modeltitle,  
            maxlik1 =NA, maxlik2 =NA, 
            AIC1 = NA, AIC2 = NA,AICc1 = NA, AICc2 = NA)
        return(output)
        } # End Failure to Converge
    
    # Otherwise,both converged so keep going
    # Record number of parameters in each component model
    nparPilot <- dim(XdesPilot)[2] + 1 #changed 8/2/06 to accurately calculate AIC (account for all parameters)
    nparPassenger <- dim(XdesPassenger)[2] + 1  #changed 8/2/06 to accurately calculate AIC (account for all parameters)
    AIC1 <- 2 * fit1$value + 2 * nparPilot
    AIC2 <- 2 * fit2$value + 2 * nparPassenger     
    
    #calculate AICc (second order information criterion) which
    #should be used for small samples (when n/(# of parameters) is
    #less than 40) according to Burnham and Anderson 200X.
    AICC1 <- AIC1 + (2*nparPilot*(nparPilot+1))/(nPilot-nparPilot-1)
    AICC2 <- AIC2 + (2*nparPassenger*(nparPassenger+1))/(nobs-nparPassenger-1)
    
    # Estimate maximum detection probability
    if (USE.INT.H==TRUE)
        {hlist <- .hInt(XdesPilot, XdesPassenger,obs = ObsYdat[, 2],
                              wMax=wMax,wb=wb,b1=b1,b2=b2,USE.PGAMMA.WB)}
    else
        {hlist <- .hNoInt(XdesPilot, XdesPassenger,obs = ObsYdat[, 2],
                              wMax=wMax,wb=wb,b1=b1,b2=b2,USE.PGAMMA.WB)}

    # Record fitted detection probability for each object
    pii <- hlist$pii

    # Calculate H-T estimator of total number of groups in search area
    Ng <- sum(1/pii)
    # H-T estimator standard error
    seNgHT <- sqrt(sum((1 - pii)/pii^2))
    
    # H-T estimator of total number of animals
    Na <- sum(size/pii)
    # H-T Std Error
    seNaHT <- sqrt(sum((1 - pii) * (size^2/pii^2)))

    # H-T estimator of total number of 'Legal sized' animals
    NaLegal<- sum(LegalSize/pii)
    

#Earl's new 'cub fix' method
#I haven't experimented with it...8/9/06
    if (UseCubFix == TRUE)
        {
        N_CubGrp <- sum(CubVec/pii)     
        N_YrlgGrp <- sum(YrlgVec/pii)
        CubCF <- max(1,N_YrlgGrp/N_CubGrp)
        #apply correction
        N_SowCub <-  sum(CubVec*size/pii)
        N_SowCubAdjust <-  CubCF*sum(CubVec*size/pii)
        Ng_adj <- Ng + (CubCF-1)*N_CubGrp
        Na_adj <- Na + (CubCF-1)*N_SowCub
        if (BootRun == FALSE)
            {
            print("Estimate corrected for sow-cub groups in den.")
            cat("  N_CubGrp ",N_CubGrp,"   N_YrlgGrp ",N_YrlgGrp," Cub Group Correction Factor = ",CubCF,"\n")  
            cat(" Uncorrected  N sow and cubs = ",N_SowCub ,"   Adjusted N sow and cubs = ",N_SowCubAdjust,"\n")
             }
          }
    else { # Not using Cub-correction
         CubCF <- 1
         Ng_adj <- Ng 
         Na_adj <- Na 
         }

    if (FALSE) #BootRun == FALSE)
        {
        Col_lab <- c("Ng","Na",paste("Pilot",sep=" ",dimnames(XdesPilot)[[2]]),"Pilot r","h1",
                paste("Passenger",sep=" ",dimnames(XdesPassenger)[[2]]),"Passenger r","h2",
                "Pilot_AIC","Passenger_AIC","NaLegal","CubCF","Ng_adj","Na_adj","W0")
        Parm_vec <- c(Ng,Na,b1,hlist$hvec[4],b2,hlist$hvec[5],AIC1,AIC2,NaLegal,CubCF,Ng_adj,Na_adj,wMax)

        modelfit <- cbind.data.frame(Col_lab,Parm_vec)
        format(modelfit, width=12, digits=3)
        print(modelfit)
        #write.table(modelfit,"testwrite", sep = "    ", row.names = FALSE)
         }

        output <- list(Ydata = cbind(ObsYdat,GroupSize=size,NLegal=LegalSize), 
            XdesPilotF = cbind(XdesPilot, pii), XdesPassengerF = cbind(XdesPassenger,pii),
             hvec = hlist$hvec, Ng = Ng, 
            Na = Na, seNgHT=seNgHT, seNaHT=seNaHT, NaLegal = NaLegal,
            CubCF =CubCF,Ng_adj =Ng_adj ,Na_adj =Na_adj, b1=b1,b2=b2,
            ### DROPPED in favor of bootstrap, Joel Reynolds 3 Oct 2008
            # SDE1 = SDE1, SDE2 = SDE2, ttest1 = ttest1, ttest2 = ttest2,
            wMax = wMax, wb = wb, modeltitle = modeltitle,  
            maxlik1 =  - fit1$value, maxlik2 =  - fit2$value, 
            AIC1 = AIC1, AIC2 = AIC2,AICc1 = AICC1, AICc2 = AICC2)
        return(output)
        
    }

`.GMRDSmodelboot` <-
function(TranData, BootTranIndex, ObsYdat, 
    XdesPilot, XdesPassenger, wMax, wb=0, startbPilot, startbPassenger,
    BearTran, UseCubFix=FALSE, CubVec,YrlgVec, 
    USE.PGAMMA.WB=FALSE,USE.INT.H=FALSE) {
# The 'Statistic' function called by boot() [library(boot)] to estimate 
# the detection model parameters for each bootstrap resample of transect data.
# This function is basically a wrapper, taking the bootstrapped 
# transect data, TranData[BootIndices,],and creating the associated 
# input objects for the estimation function .GMRDSmodel
##########
# CALLED FROM: MainScriptBootstrap.r => BootstrapGMRDSmodel 
# CALLS:       .GMRDSmodel
####### ARGUMENTS ####### 
# TranData:     AT LEAST 3 column data frame of ALL transect data as follows:
#                    TransNo, Length,Area
# BootTranIndex Index vector of bootstrap replicate sample of transects
# ObsYdat:      5 column matrix containing : Bear id,
#                obsr(1-Pilot only, 2-Passenger only, 3-both), 
#                 Distance from the transect to the bear;
#                 Vector of number of bears in each group (size),
#                 Vector of number of legal bears in each group (n - cubs - yrlings) (LegalSize)
#                 Rows align w/ Transect ID ('TransNo') in BearTran. 
# XdesPilot:    The Pilot's DESIGN MATRIX (must match up with ObsYdat)
#                column1=vector of ones denoting the intercept term
#                column2 ... - rest of design matrix for the Pilots detection model
# XdesPassenger: The Passenger's DESIGN MATRIX (must match up with ObsYdat)
#                column1=vector of ones denoting the intercept term
#                column2 ... - rest of design matrix for the Passengers detection model
# wMax          Effective strip width - set by user in MainScriptModelFit.R 
# wb            Blind strip width - use 22 m for Supercub 100 m. agl (above ground level)
# startbPilot:    initial guesses for Pilot's model coefficients
# startbPassenger: initial guesses for Passenger's  model Coefficients       
# BearTran:     Vector of Tansect IDs ('TransNo') for each animal group (aligned w/ ObsYdat).
# UseCubFix     If T then adjust estimates so that 
#                # cub Groups >= # Yearling Groups
# CubVec        Vector of 0,1's denoting Sow with cubs
# YrlgVec        Vector of 0,1's denoting Sow with yearlings
# USE.PGAMMA.WB  Logical (default FALSE) - controls use of second pgamma in likelihood (see .GammaLnTrns.lik)
# USE.INT.H      Logical (default FALSE) - controls estimation algorithm for h.
##########################################
# Author: Earl Becker and Pham Quong, (dates)
# Ported to R: Earl Becker and Aaron Christ 9/19/04
# Additional documentation: Joel Reynolds 11/05 - 5/09
# Revision for computational efficiency: Joel Reynolds 11/05 
# Minor renaming for ease of access in returned objects: Joel Reynolds 11/05
# Revision as wrapper statistic called by library(boot)'s boot() function,
#   minor revision to align function arguments for consistency: 
#   Brook Russel 7/06; Joel Reynolds 10/08
####################
# Last Edit JR 29 May 2009 - modification of arguments, etc.
###################

# Called from boot()
  ###########
  # create bootstrap arguments for .GMRDSmodel
  ###########
  
  # change 6/26/06...Area (search area for each transect)
  areakm2 <- sum(as.numeric(TranData$Area[BootTranIndex]))
  
  # drop transects with no bear observations (play no role in detection function estimation).
  # gives a vector of indices of transects with bear sightings. 
  BootTranIndex<-BootTranIndex[is.element(TranData$TransNo[BootTranIndex], BearTran)]
  
  # Pull out Bootstrap Replicate bear group observation data by matching Transect IDs.
  # This line only pulled out one group of bears on transects with multiple
  # bear group sightings!!!
  # ObsYdat.boot<-ObsYdat[match(TranData$TransNo[BootTranIndex],BearTran),]
  ###################
  # change 6/27/06...allows for multiple groups on same transect.
  # Note:  BearTran must match up with ObsYdat (as dictated in the arguments section).
  #         'merge' wants data frames, not matrices
    ObsYdat.boot<-merge(data.frame(ObsYdat,"BearTran"=BearTran),data.frame("TrashIndex"=TranData$TransNo[BootTranIndex]),
        by.x="BearTran",by.y="TrashIndex")[,2:6]

  # Pull out bootstrap replicate design matrices
  # These lines are only pulling out one bear group per transect
  # XdesP.boot<-XdesPilot[match(TranData$TransNo[BootTranIndex],BearTran),]
  # XdesO.boot<-XdesPassenger[match(TranData$TransNo[BootTranIndex],BearTran),]
  ###################
  # change 6/27/06...should allow for multiple group sightings on same transect.
  # Note:  XdesPilot and XdesPassenger must match up with ObsYdat!!!
  
  XdesP.boot<-merge(data.frame(XdesPilot,"BearTran"=BearTran),data.frame("TrashIndex"=TranData$TransNo[BootTranIndex]),
        by.x="BearTran",by.y="TrashIndex")[,2:(ncol(XdesPilot)+1)]
  XdesO.boot<-merge(data.frame(XdesPassenger,"BearTran"=BearTran),data.frame("TrashIndex"=TranData$TransNo[BootTranIndex]),
        by.x="BearTran",by.y="TrashIndex")[,2:(ncol(XdesPassenger)+1)]
  
  # It wants matrices instead of dataframes (6/27/06).
  XdesP.boot<-as.matrix(XdesP.boot)
  dimnames(XdesP.boot)[[2]]<-dimnames(XdesPilot)[[2]]

  XdesO.boot<-as.matrix(XdesO.boot)
  dimnames(XdesO.boot)[[2]]<-dimnames(XdesPassenger)[[2]]
  
  
  #gives .GammaLnTrns.N 0-1 cub and yearling vectors.  they need to be
  #called CubVec and YrlgVec
  CubVec.boot<-merge(data.frame(CubVec,"BearTran"=BearTran),data.frame("TrashIndex"=TranData$TransNo[BootTranIndex]),
        by.x="BearTran",by.y="TrashIndex")[,2]
  YrlgVec.boot<-merge(data.frame(YrlgVec,"BearTran"=BearTran),data.frame("TrashIndex"=TranData$TransNo[BootTranIndex]),
        by.x="BearTran",by.y="TrashIndex")[,2]
  

  # 6/23/06  Fit detection functions for bootstrap resample observations.
  #        NOTE argument BootRun=TRUE
  out <-  .GMRDSmodel(ObsYdat=ObsYdat.boot[,c(1:3)], XdesPilot=XdesP.boot, XdesPassenger=XdesO.boot, 
          wMax=wMax, wb=wb,startbPilot=startbPilot, startbPassenger=startbPassenger, 
          modeltitle=NULL, size=ObsYdat.boot[,4], LegalSize=ObsYdat.boot[,5], 
          BootRun=TRUE,USE.PGAMMA.WB=USE.PGAMMA.WB, USE.INT.H=USE.INT.H,UseCubFix=UseCubFix,
          CubVec=CubVec.boot,YrlgVec=YrlgVec.boot)

    #browser()
  # if estimation failed to converge, return NAs
  if (is.na(out$Na)) 
    {  output<-c(areakm2,out$b1,out$b2,out$hvec,
                 NA, NA, NA,NA,NA,NA)
    }
  # If estimation converged, return results   
  else  
     { output<-c(areakm2,out$b1,out$b2,out$hvec,
                out$Ng, out$Na, out$NaLegal,
                (1000*out$Ng/areakm2),(1000*out$Na/areakm2),(1000*out$NaLegal/areakm2))
     }
       
    names(output)<-c("Area",paste("Pilot",c(dimnames(XdesPilot)[[2]],"r"),sep=""),
                paste("Passenger",c(dimnames(XdesPassenger)[[2]],"r"),sep=""),
                "n1","n2","n3","PilotMaxDetect","PassengerMaxDetect", #hvec
                "Ng","Na","NaLegal","NgDensityper1000KM2","NaDensityper1000KM2","NaLegalDensityper1000KM2")
                
    # Reorder output for easier access later
    # Ng, Na, NaLegal, NgDens, NaDens, NaLegDens,PilotMaxDetect,PassengerMaxDetect,Pilot coeff, Passenger coeff,n1, n2, n3, area
    trashn<-length(output)
    ReorderSequence<-c(trashn-c(5:0,7,6),2:(trashn-11),trashn-10,trashn-9, trashn-8,1)
    output<-output[ReorderSequence]
    
  return(output)
}

`.b` <-
function(Ydat, Xdes, obs, start,wMax,wb,USE.PGAMMA.WB=F)
{
# Called by .GLTmodel to fit detection function for given observer (Pilot/Passenger?)
# and given detection model (Xdes - design matrix).
########## INPUT ########
# Ydat - bear group observations: distance to group from transect (LTdata$DistanceOffTransect)
# Xdes - design matrix for model & observer platform (Pilot or Passenger)
# obs  - observation platform of interest: 1 - Pilot, 2 - Passenger, 3 - BOTH
# start - vector of initial estimates for detection model parameters
# wMax, wb - max search distance and minimum dist (width of blind strip)
# USE.PGAMMA.WB - flag for likelihood optimization routine.
# BootRun -Logical - controls initial assignment of objective and gradient functions to
#   active workspace.
################ OUTPUT ###########
# list with components 
#     $par - optimal parameter estimates: (covariate model: intercept, slopes; gamma shape parameter r)
#     $value - optimal value of objective function
#     $hessian - estimated hessian at optimal parameterization
#     $convergence - error message on convergence (see help(optim) for interpretation) 
######################
# Original code: 
#   Earl Becker, Pham Quong (dates)
#       earl_becker@fishgame.state.ak.us, ??
# ported to R:
#   Aaron Christ (dates)
#       aaron_christ@fishgame.state.ak.us
###
# Further documentation and edits: 
#   Joel Reynolds Dec 05
# Last Edit JR 23 June 2006
#   joel_reynolds@fws.gov
#####################################

# extract relevant data for this observation platform
    dat <- .pre(Ydat = Ydat, Xdes = Xdes, observer = obs,wMax=wMax,wb=wb)
    # fit detection model using trust function
    fit <- .trustGMRDS(dat, start,USE.PGAMMA.WB=USE.PGAMMA.WB)
    fit
    }

`.trustGMRDS` <-
function(dat, st0, h = 0.0001,USE.PGAMMA.WB=FALSE)
{
##################### 
# This function is called by .b to find maximum likelihood estimates.
####### ARGUMENTS #######
# dat   A list created by .pre with components n: the number of groups
#       observed by Pilot/Passenger, y: a vector of distances to transect,
#       X.: the design matrix, observer: 1 for pilot and 2 for passenger,
#       wMax and wb.
#
# st0   A vector of the following form: intercept, param. 1, ..., param. p, r.
#       These are the starting values for the trust function.  They are also
#       used by the trust function as an estimate of the relative scale of
#       the parameters.  As the trust function is very sensitive to starting
#       values, this vector should be as accurate as possible.  If the trust
#       function crashes as it attempts to converge to an r value of 1 (recall
#       that r>=1, but r=1 results in the classic monotonic detection
#       function), then you need a better guess for st0.  This can be changed
#       in the startbPilot and startbPassenger vectors.
#
# h
#
# USE.PGAMMA.WB is logical telling wheter to use second pgamma term in likelihood
# (see lt7.lik function)--    defaults to false.
#####################
# Author: Earl Becker and Pham Quong, (dates)
# Ported to R: Earl Becker and Aaron Christ
# Additional documentation: Brook Russell, Joel Reynolds (July 06)
####################
# References:
#
#
###################
# Last Edit 21 July 2006
###################
# The trust package must be loaded in user the script.

    X. <- dat$X.
    y <- dat$y
    
    assign(".h", h, pos =1)

    .hessf <- function(b, ...)
    {    
    
        if (!(b[length(b)] >= 1)) return(list(value = Inf))
        
        temp<-fdHess(pars=b,fun=function(x,...){.likGMRDS(b2 = x,...)},...)
        names(temp)[c(1,3)]<-c("value","hessian")
        return(temp)
    }
    
    assign(".hessf", .hessf, pos =1)  
    
    pscl<-abs(st0)
#you must give the trust function a 'good' estimate of the parameters in st0.
    value <- trust(objfun=.hessf,parinit=st0,rinit=.1,rmax=.3,X.=X.,y=y,w=dat$wMax,wb=dat$wb,USE.PGAMMA.WB=USE.PGAMMA.WB,blather=F,parscale=pscl,iterlim=1500)
#The parscale argument is used to attempt to put all variables on the same scale.  The function
#essentially divides by the reasonable estimate which you supply in the vector st0.  These estimates
#are also used as initial values.  
#For large models, the function can require many (up to a thousand
#or more) iterations to converge.  For this reason the iterlim argument is set fairly high.

#added 7/17/06 to alert user if the trust function doesn't converge.  Doesn't 
#if(value$converged==FALSE){print("The trust function did not converge.");value$argument<-NULL}
# Note FOR debugging purposes change blather=T in 'trust' call (above) and use cat("value\n",value)below to see optimization history
#print(value)

}

`.hInt` <-
function(XdesPilot, XdesPassenger, obs,wMax,wb,b1,b2,USE.PGAMMA.WB=FALSE)
{# Called by LT6.N to estimate h, the peak detection probability for given observer (Pilot/Passenger)
#  and given detection model (Xdes - design matrix) and parameter estimates.
#  Uses integration to estimate Aii, B (vrs approximation in LT6.h.noInt).
##########
# XdesPilot / XdesPassenger - design matrices of detection models for each observer platform (Pilot / Passenger)
# obs  - vector, who observed group: 1 - Pilot only, 2 - Passenger only, 3 - BOTH
# wMax, wb - max search distance and minimum dist (width of blind strip)
# b1, b2 - MLE of detection function parameters for Pilot (b1) and Passenger (b2).
# USE.PGAMMA.WB - flag for likelihood optimization routine.
######################
# Original code: 
#   Earl Becker, Pham Quong (dates)
#       earl_becker@fishgame.state.ak.us, ??
# ported to R:
#   Aaron Christ (dates)
#       aaron_christ@fishgame.state.ak.us
###
# Further documentation and edits: 
#   Joel Reynolds Dec 05
# Last Edit 23 June 2006
#   joel_reynolds@fws.gov
#####################################
# col1=id, col2=obsr, col3=x, col4=size are obligatory  
# optional covariates start at col 5: col5=activ1, col6=activ2, ... 
    n1 <- sum(obs == 1)
    n2 <- sum(obs == 2)
    n3 <- sum(obs == 3)

    lam1 <- exp(XdesPilot%*% b1[ - length(b1)])
    lam2 <- exp(XdesPassenger%*% b2[ - length(b2)])

    wb. <- wb ; #dummy version so we can zero it out
#for now, make wb zero
    wb <- 0 
    A <- apply(lam1,MARGIN =1, FUN=.intA, w = wMax, wb = wb, r = b1[length(b1)],USE.PGAMMA.WB)
    Ai1 <- (1/wMax)*unlist(lapply(A,FUN=function(x) {x$value }))
    A <- apply(lam2,MARGIN =1, FUN=.intA, w = wMax, wb = wb, r = b2[length(b2)],USE.PGAMMA.WB)
    Ai2 <- (1/wMax)*unlist(lapply(A,FUN=function(x) {x$value }))
    B<- apply(cbind.data.frame(lam1,lam2),MARGIN =1, FUN=.intB, w = wMax, wb = wb,
            r11=b1[length(b1)],r22=b2[length(b2)],USE.PGAMMA.WB)        
    Bi12 <- (1/wMax)*unlist(lapply(B,FUN=function(x) {x$value }))
    # ------------ New code for EM algo
    pii0 <- rep(1, length(Ai1))
    pii <- (Ai1 + Ai2 - Bi12)
    while(max(abs(pii - pii0)) > 1.0000000000000001e-005) {
        pii0 <- pii
        A1 <- sum(Ai1/pii0)
        A2 <- sum(Ai2/pii0)
        B12 <- sum(Bi12/pii0)
        h2 <- (n3 * A1)/((n1 + n3) * B12)
        h1 <- (n3 * A2)/((n2 + n3) * B12)
        pii <- (h1 * Ai1 + h2 * Ai2 - h1 * h2 * Bi12)
        pii <- pmin(pmax(1.0000000000000001e-005, pii), 1)
    }
    h1 <- min(h1, 1)
    h2 <- min(h2, 1)
    # ------------ End new code for EM algo 

    list(hvec = c(n1,n2,n3,h1,h2), IntVec = matrix(c(A1,A2,B12),ncol=3,byrow=FALSE),pii = pii)

}

`.hNoInt` <-
function(XdesPilot, XdesPassenger, obs,wMax,wb, b1, b2,USE.PGAMMA.WB=FALSE)
{
# Called by .GLTmodel to estimate h, the peak detection probability for given observer (Pilot/Passenger)
#  and given detection model (Xdes - design matrix) and parameter estimates.
##########
# XdesPilot / XdesPassenger - design matrices of detection models for each observer platform (Pilot / Passenger)
# obs  - vector, who observed group: 1 - Pilot only, 2 - Passenger only, 3 - BOTH
# wMax, wb - max search distance and minimum dist (width of blind strip)
# b1, b2 - MLE of detection function parameters for Pilot (b1) and Passenger (b2).
# USE.PGAMMA.WB - flag for likelihood optimization routine.
######################
# Original code: 
#   Earl Becker, Pham Quong (dates)
#       earl_becker@fishgame.state.ak.us, ??
# ported to R:
#   Aaron Christ (dates)
#       aaron_christ@fishgame.state.ak.us
###
# Further documentation and edits: 
#   Joel Reynolds Dec 05
# Last Edit 23 June 2006
#   joel_reynolds@fws.gov
#####################################    
    n1 <- sum(obs == 1) # detected only by Pilot
    n2 <- sum(obs == 2) # detected only by Passenger
    n3 <- sum(obs == 3) # detected by both
    lam1 <- exp(XdesPilot %*% b1[ - length(b1)])
    lam2 <- exp(XdesPassenger %*% b2[ - length(b2)])
    r1 <- b1[length(b1)]
    r2 <- b2[length(b2)]
# Differs from .hInt
    bb1 <-  (1/gamma(r1))*(((r1 - 1)/exp(1))^(r1 - 1))
    bb2 <-  (1/gamma(r2))*(((r2 - 1)/exp(1))^(r2 - 1))
    beta1 <- bb1*lam1
    beta2 <- bb2*lam2
    beta <- beta1*beta2/(beta1 + beta2)
    denom <- (wMax-wb)*gamma(r1)*gamma(r2)*(bb1^r1)*(bb2^r2)*(lam1^(r1-1))*(lam2^(r2-1))

    if (USE.PGAMMA.WB) {
            Ai1 <- (lam1/(wMax-wb))*(pgamma(wMax/beta1, r1) - pgamma(wb/beta1, r1)) 
            Ai2 <- (lam2/(wMax-wb))*(pgamma(wMax/beta2, r2) - pgamma(wb/beta2, r2))
            Bi12 <- (beta^(r1+r2-1))*gamma(r1+r2 -1)*(pgamma(wMax/beta,(r1+r2-1))-pgamma(wb/beta,(r1+r2-1)))/denom      
    } else
            { # eq. 9 and 11 in Becker and Quang 2008
            Ai1 <- (lam1/wMax)*pgamma(wMax/beta1, r1)
            Ai2 <- (lam2/wMax)*pgamma(wMax/beta2, r2)
            Bi12 <- (beta^(r1+r2 -1))*gamma(r1+r2 -1)*pgamma(wMax/beta,(r1+r2 -1))/denom
            }
            
    # ------------ New code for EM algo
    pii0 <- rep(1, length(Ai1))
    pii <- (Ai1 + Ai2 - Bi12)
    while(max(abs(pii - pii0)) > 1.0000000000000001e-005) {
        pii0 <- pii
        A1 <- sum(Ai1/pii0)
        A2 <- sum(Ai2/pii0)
        B12 <- sum(Bi12/pii0)
        h2 <- (n3 * A1)/((n1 + n3) * B12)
        h1 <- (n3 * A2)/((n2 + n3) * B12)
        
        # pii calculation below is eq. 22 in Becker and Quang 2009
        pii <- (h1 * Ai1 + h2 * Ai2 - h1 * h2 * Bi12)
        pii <- pmin(pmax(1.0000000000000001e-005, pii), 1)
    }
    h1 <- min(h1, 1)
    h2 <- min(h2, 1)
    # ------------ End new code for EM algo
    
    list(hvec = c(n1,n2,n3,h1,h2), IntVec = matrix(c(Ai1,Ai2,Bi12),ncol=3,byrow=FALSE),pii = pii)
}

`.intA` <-
function(lam, w, wb, r,USE.PGAMMA.WB=FALSE)
{
# Called from .hInt
# Calculates A*=integral K(y/lam)dy over [0,w]
# Note A = (1/w)A*
    Ky <- function(x, lam2 = lam, r2 = r)
    {
        gammar <- gamma(r2)
        B <- (1/gammar) * ((r2 - 1)/exp(1))^(r2 -
            1)
        y1 <- x/(lam2 * B)
        (((y1^(r2 - 1))) * exp( - y1))/(gammar * 
            B)
    }
    if (USE.PGAMMA.WB==FALSE)
        wb1<- 0
    else
        wb1 <- wb
    integrate(Ky, lower = wb1, upper = w, stop.on.error =FALSE,lam2 = lam, r2 = r)
#   integrate(Ky, lower = wb1, upper = wMax, stop.on.error =FALSE, abs.tol = 0.000005,lam2 = lam, r2 = r)
}

`.intB` <-
function(lam, w ,wb,r11,r22,USE.PGAMMA.WB = FALSE)
{
# Called from .hInt
###
#  intB.N3 - changed by EB 9-20-00 to ro calcs. w/ rhat1 and rhat2
# Calculates B*=integral K(y/lam1)K(y/lam2)dy over [wb,w]
#   Note B = (1/w)B*
    Ky <- function(x, lam21 = lam[1], lam22 = lam[2
        ], r11 , r22)
    {
        B1 <- ((r11 - 1)/exp(1))^(r11 - 1)/
            gamma(r11)
        B2 <- ((r22 - 1)/exp(1))^(r22 - 1)/
            gamma(r22)
        y1 <- x/(lam21 * B1)
        y2 <- x/(lam22 * B2)
        (((y1)^(r11 - 1)) * ((y2)^(r22 - 1)) * 
            exp( - y1) * exp( - y2))/(gamma(
            r11) * B1 * gamma(r22) * B2)
    }
    if (USE.PGAMMA.WB==FALSE)
        wb1<- 0
    else
        wb1 <- wb
#   integrate(Ky, lower = 0, upper = w, stop.on.error =FALSE, lam21 = lam[1], lam22 = lam[2], 
#   integrate(Ky, lower = wb, upper = w, stop.on.error =FALSE, abs.tol = 0.000005, lam21 = lam[1], lam22 = lam[2], 
    integrate(Ky, lower = wb1, upper = w, stop.on.error =FALSE, lam21 = lam[1], lam22 = lam[2], 
        r11 = r11, r22 =r22)
}

`.likGMRDS` <-
function(b2, X., y, w, wb ,USE.PGAMMA.WB=FALSE,S=0)
{
####################
### ARGUMENTS ###
#
# b2        vector of model coefficient and r gamma shape parameter
#
# X.        design matrix for pilot/passenger
#
# y         ObsYdat
#
####################
# This functions returns Obj(b2)=-loglik(b2), the objective
# to be minimized by .trustGMRDS
#            Equation 8 in Becker and Quang 2008 
#defaults to leaving out the pgamma(w1b,r) term


    r <- b2[length(b2)]
    bb <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
    J <- dim(X.)[2]
    eta <- X. %*% matrix(b2[1:(length(b2)-1)],ncol=1)
    lam <- exp(eta)
    v1 <- y/(lam * bb)
    w1 <- w/(lam * bb)
    w1b <- wb/(lam * bb)

if (USE.PGAMMA.WB)
 loglik <- sum((r - 1) * log(v1) - v1 - lgamma(r) - log(bb) - eta - log(pgamma(w1, r)-pgamma(w1b,r)))
else 
    loglik <- sum((r - 1) * log(v1) - v1 - lgamma(r) - log(bb) - eta - log(pgamma(w1, r)))
    #computationaly more efficient form of likelihood than eq. 8 in Becker and Quang 2008
#end if (USE.PGAMMA.WB)

#note that when wb is zero pgamma(w1b,r) will also be zero so
#     no blind strip in intergration 

#cat("  like= ",loglik,"\n")
if (is.na(loglik))
{     print("r = ")
      print(r)
      print("bb = ")
      print(bb)
      cat(" v1 =",v1,"\n")
      cat("b= ", b2, "\n")
      cat(" lam = ",lam,"\n")
      cat("(r-1)*log(v1) = ",sum((r - 1) * log(v1)),"    ","\n")
      cat("sum v1 = ",sum(v1),"    ","\n")
      cat("lgamma(r) = ",(lgamma(r)),"    ","\n")
      cat("log(bb) = ",(log(bb)),"    ","\n")
      cat("sum eta = ",sum(eta),"   ","\n")
      cat("sum log(pgamma(w1, r)) = ",sum(log(pgamma(w1, r))),"     ","\n")
      cat("sum log(pgamma(w1b,r)) = ",sum(log(pgamma(w1b,r))),"\n")
    if(sum(log(pgamma(w1, r)))!=0)
      cat("log(pgamma(w1, r)) = \n",(log(pgamma(w1, r))),"\n")
    if(sum(log(pgamma(w1b, r)))!=0)
      cat("log(pgamma(w1b,r)) = \n",(log(pgamma(w1b,r))),"\n")
}

if (loglik==Inf){
    #try a trick to use the biggest number instead of Inf so L-BFGS-B doesn't crash
    loglik = .Machine$double.xmax 
    }
     - loglik
}

`.pre` <-
function(Ydat, Xdes, observer,wMax,wb)
{
####################
### ARGUMENTS ###
#
# Ydat      The matrix ObsYdat passed to .pre as Ydat.
#
# Xdes      The Pilot/Passenger design matrix is passed to
#           .pre as Xdes.
# obs - column: who saw group? 1 = Pilot only; 2 = Passenger Only, 3 = seen by both
# wMax
# wb
#####################
####################
# Called from .b. 
# Sets up X design matrix and Y (observations) for fitting detection 
# function for observations platform:
# (observer==1 : Pilot; == 2 : Passenger; ==3 : Both(?))
######################
# Original code: 
#   Earl Becker, Pham Quong (dates)
#       earl_becker@fishgame.state.ak.us, ??
# ported to R:
#   Aaron Christ (dates)
#       aaron_christ@fishgame.state.ak.us
# documentation by Joel_Reynolds@fws.gov 5/06
###################
# Last Edit 21 July 2006
###################

    obs <- Ydat[, 2]    
    if(observer == 1) {                             # Groups seen by Pilot
        X <- Xdes[(obs == 1) | (obs == 3),  ]       #pull out relevant transect covariatess
        y <- Ydat[(obs == 1) | (obs == 3), 3]       #pull out distance to group (DistanceOffTransect).
    }
    if(observer == 2) {                             # seen by Passenger
        X <- Xdes[(obs == 2) | (obs == 3),  ]
        y <- Ydat[(obs == 2) | (obs == 3), 3]
    }
    if(observer == 3) {                             # seen by both.
        X <- Xdes[(obs == 3),  ]
        y <- Ydat[(obs == 3), 3]
    }

    n <- length(y)  
    list(n = n, y = y, X. = X, observer = 
        observer,wMax=wMax,wb=wb)
}
