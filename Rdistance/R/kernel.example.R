# Instructions to run a kernel density estimate of a distance detection function using R.
# From Ryan Nielson, WEST
# Modified by Jason Carlisle



# Set working directory to the CarlisleWorkspace branch saved on local machine
# This will be replaced with the GitHub option above once I figure it out.
packdir <- "C:/R_Code/Rdistance/Rdistance"  # Carlisle laptop



# Load example dataset (one data.frame with column of perpendicular distances named 'distance')
load(paste(packdir, "data", "tees.rda", sep="/"))

hist(tees$distance, breaks=20, col="grey")


# Source current functions (under development)
source(paste(packdir, "R", "kernel.analysis.R", sep="/"))
source(paste(packdir, "R", "F.df.adaptive.boot.R", sep="/"))  # the input 'statistic' for the R boot function to compute confidence intervals for N (population size)








#-----------------------------------------------------------------------------------------------------------------------------------
  
#   How to run:
#   
#   1.) Select the bandwidth method in kernel_analysis.r file by uncommenting the appropriate code and commenting out methods not chosen
# , then source in the file kernel_analysis again. (currently 5 phase plugin is selected. jg/11/5/10).

# 2.) Run the code. 
F.DF_adaptive(obs.data=tees,W1=0,W2=4,n=142,stripwidth=4,transect_length=210)

# For this example W1 is minimum sighting distance, W2 is max sighting distance, stripwidth is the half-width as observations
# were made on only one side of the transect line.

# 3.) Run bootstrap for bootstrap CI's for population size N by pasting the code below into R one line at a time.
# requires boot package
if(!require(boot)) { 
  install.packages("boot")
  require(boot)
}


tees.dpi.boot<- boot(data=tees,statistic=F.DF_Adaptive_boot,W1=0,W2=4,n=142,stripwidth=4,transect_length=210,R=1000)
tees.dpi.boot.ci<- boot.ci(tees.dpi.boot,conf= 0.95,type="all")
tees.dpi.boot.ci





