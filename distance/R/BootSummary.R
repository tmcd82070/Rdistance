`BootSummary` <-
function(bootObject,CItype=c("bca","perc"),CONF=0.95,ROUND=4)
{
#####################
# This function is called from the third user script, MainScriptBootStrap,
# to calculate various summaries of the bootstrap replicate sampling distributions.
####### ARGUMENTS #######
# bootObject   An object created by a successful call to the boot() function.
# CItype  desired bootstrap confidence interval types (Bias-Corrected accelerated, Percentile)
# CONF  desired confidence level
# ROUND number of decimal places to the right of '.' to keep in rounding results.
#####################
# Author: Joel_Reynolds@fws.gov (May 09)
####################
# Last Edit 4 June 2009
###################

 if (missing(bootObject)) stop("Missing object created by call to boot() function.")
  
# The boot package must be already loaded.
## if not already installed, add library(boot)
  bootAvailable<-require("boot", quietly=TRUE, character.only=TRUE)
  if (!bootAvailable) stop("Requires installation of R package 'boot'.\nPlease download from CRAN via the 'Packages =>Install Package' menu.")  
  
  # Any replicates that failed to converge?
  if (sum(is.na(bootObject$t))) stop("Results for some bootstrap replicates are 'NA'.")
     
    BootSummarydf<-data.frame("Parameter"=names(bootObject$t0),"Est"=bootObject$t0,"Bias"=NA,"RelativeBias"=NA,"StdError"=NA,"CV"=NA)
    
    #  ESTIMATOR BIAS for each parameter of interest
    BootSummarydf$Bias<-colMeans(bootObject$t, na.rm=TRUE)-bootObject$t0

    # Relative Bias 
    # (bias as fraction of true value; 0.05 is bias of 5%)
    BootSummarydf$RelativeBias<-BootSummarydf$Bias/bootObject$t0

    #STANDARD ERRORS
    BootSummarydf$StdError<-sqrt(apply(bootObject$t,2,var, na.rm=TRUE))
 
    # Coefficients of Variation
    #  estimates (CV=SE/est)
    BootSummarydf$CV <- BootSummarydf$StdError/bootObject$t0

    # d)  CONFIDENCE INTERVALS
    Numrows<-dim(bootObject$t)[2]
    boot.conf.ints<-matrix(NA,nrow=Numrows,ncol=length(CItype)*2)
    
       

    for (i in 1:Numrows){ # for each thing being estimated
        trash.ci<-boot.ci(bootObject,index=i,type=CItype,conf=CONF)
        if (is.null(trash.ci)) stop('CI calculations crashed on ',names(bootObject$t0)[i],'. Just use percentile intervals? (CItype="perc")' )
        
        if (length(CItype)==1)
            boot.conf.ints[i,]<- if ('bca'%in%CItype) trash.ci$bca[4:5] else trash.ci$percent[4:5] #only one type selected
        else    # both types selected
            boot.conf.ints[i,]<-c(trash.ci$bca[4:5],trash.ci$percent[4:5])
    }
    
    # put it all together in a data frame object
    dimnames(boot.conf.ints)[[2]]<- {if (length(CItype)==1) {if ('bca'%in%CItype) c("LowerBCa","UpperBCa") else c("LowerPercentile","UpperPercentile")}
                                    else    # both types selected
                                    c("LowerBCa","UpperBCa","LowerPercentile","UpperPercentile")}
    row.names(boot.conf.ints)<-NULL 
    BootSummarydf<-cbind(BootSummarydf,boot.conf.ints)
    row.names(BootSummarydf)<-NULL
    BootSummarydf[,-1]<-round(BootSummarydf[,-1],ROUND)
return(BootSummarydf)

}

