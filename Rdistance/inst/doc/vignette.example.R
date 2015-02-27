# Vignette for the Rdistance Package
# Distance-sampling analysis in Rdistance

# Prepared by:
# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit, University of Wyoming
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com
# Assistance from Trent L. McDonald, WEST, Inc.

# Last updated 2/27/2015

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Install package ---------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## USE CURRENT GITHUB VERSION OF THE PACKAGE
# Install devtools in order to install development version of Rdistance from GitHub
require(devtools)

# Some functionality of devtools depends on Rtools.  install_github() apparently does not.
# If needed though, download Rtools from web at http://cran.r-project.org/bin/windows/Rtools/
# For R version 3.1.2, use Rtools31.exe

# Install the development version of Rdistance from the CarlisleWorkspace branch on GitHub
install_github("tmcd82070/Rdistance/Rdistance@CarlisleWorkspace")

require(Rdistance)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Import and prep data ----------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Load the example sparrows dataset from package
data(sparrows)

# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
dists.df$dists <- perp.dists(obs.dist=dists.df$sightdist, obs.angle=dists.df$sightangle, digits=1)

# Remove sight distance and angle
dists.df <- dists.df[, -which(names(dists.df) %in% c("sightdist", "sightangle"))]

# Save vector of distances
x <- dists.df$dists

# Examine the histogram of distances
hist(x)
rug(x)
summary(x)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit a detection function ------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit detection function
# Requires only a vector of distances
?F.dfunc.estim  # outdated -- need to update that dist argument can take two forms (see below)
(dfunc <- F.dfunc.estim(x, likelihood="halfnorm", w.hi=150))  # supplying just the vector of distances

# same as supplying the data.frame with "dists" column
F.dfunc.estim(dists.df, likelihood="halfnorm", w.hi=150)



# Explore the dfunc object
dfunc$parameters
dfunc$call
dfunc$fit
dfunc$w.lo
dfunc$w.hi
# How to extract the ESW?  It prints, but how would one extract the value?
ESW(dfunc)  # found it, didn't realize it wasn't stored in dfunc directly


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Estimate abundance given a detection function ---------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Requires two data.frames:
  # a distance data.frame with at least $distance, $siteID, and $groupsize
  # a site-covariate data.frame with at least $siteID and $length


# 72 transects surveyed, each 500 m
# area=10000 converts to density per ha (for dist measured in m)


?F.abund.estim  # really outdated -- group sizes and transect lengths etc. handled differently (see inside function)

(fit <- F.abund.estim(dfunc, distdata=dists.df, covdata=covs.df, area=10000,
                      R=500, ci=0.95, plot.bs=TRUE, bs.method="transects"))



# Explore the fit object
str(fit)

# Abundance estimate and CI
fit$n.hat
fit$ci

# Distribution of the bootstrap replicates of the abundance estimate
hist(fit$B)


# plot
plot(fit)








#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Plot abundance estimate and CI ------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Thinking of building in a barplot with CI or something ...
?barplot










## Next sections to tackle



# ?F.abund.estim
# ?F.gx.estim
# 
# # Throws error prior to bootstrapping:
# #Error in key * (1 + (exp.term %*% a[2:(nexp + 1)])) : 
# #    non-conformable arrays
# F.automated.CDA(x, area=10000, total.trans.len=500, w.hi = 150,
#                 likelihoods=c("uniform", "halfnorm", "hazrate", "negexp", "Gamma"),
#                 series=c("simple", "cosine"), expansions=c(0, 1, 2, 3), plot=TRUE)
# 
# # uniform, 0 expansions selected as best
# # density (per ha) is 0.96 (CI=0.84, 1.19)
# F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi = 150,
#                 likelihoods=c("uniform", "halfnorm", "Gamma"),
#                 series=c("simple", "cosine"), expansions=c(0, 1, 2), plot=TRUE)
# 
# F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi=150,
#                 likelihoods=c("halfnorm", "uniform", "hazrate", "negexp", "Gamma"),
#                 series=c("simple", "cosine"), plot=TRUE)
# 










## Junk code
# # Set working directory
# setwd("C:\\Users\\jcarlis3\\Box Sync\\Classes\\s8_Spr_15\\DistanceSampling\\ExampleVignette")


# # Read in example datasets
# counts <- read.csv("dists.df.csv")
# covs <- read.csv("Sparrows.Covariates.csv")


# # Set working directory to the CarlisleWorkspace branch saved on local machine
# # This will be replaced with the GitHub option above once I figure it out.
# packdir <- "C:/R_Code/Rdistance/Rdistance"  # Carlisle laptop
# packdir <- "C:/Users/tmcdonald/Google Drive/Documents/Programs/Rdistance/Rdistance"
# 
# 
# # Load example dataset (two data.frames)
# load(paste(packdir, "data", "sparrows.rda", sep="/"))
# 
# # data(sparrows)
# 
# 
# 
# 
# 
# # Source current functions (under development)
# source(paste(packdir, "R", "perp.dists.R", sep="/"))
# source(paste(packdir, "R", "F.dfunc.estim.R", sep="/"))


# # Load or Install/load the Rdistance package (currently version 1.1)
# if(!require(Rdistance)) { 
#     install.packages("Rdistance")
#     require(Rdistance)
# }

# # Add NA rows to counts where no sparrows were observed
# length(unique(sparrows.covs$TranID))  # unique IDs for all transects surveyed (even if no sparrow recorded)
# (absences <- data.frame(table(dists.df$TranID)))  # Note sparrows not observed at all transects
# (absences <- as.character(absences[absences$Freq==0, 1]))
# 
# (toadd <- data.frame(matrix(nrow=length(absences), ncol=ncol(dists.df))))
# toadd[, 1] <- absences
# names(toadd) <- names(dists.df)
# 
# # Append NA rows
# dists.df <- rbind(dists.df, toadd)