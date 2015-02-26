# Vignette for the Rdistance Package
# Distance-sampling analysis in Rdistance

# Prepared by:
# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit, University of Wyoming
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com
# Assistance from Trent L. McDonald, WEST, Inc.

# Last updated 2/25/2015




## USE CURRENT GITHUB VERSION OF THE PACKAGE
# Install devtools in order to install development version of Rdistance from GitHub
require(devtools)

# Some functionality of devtools depends on Rtools.  install_github() apparently does not.
# If needed though, download Rtools from web at http://cran.r-project.org/bin/windows/Rtools/
# For R version 3.1.2, use Rtools31.exe

# Install the development version of Rdistance from the CarlisleWorkspace branch on GitHub
install_github("tmcd82070/Rdistance/Rdistance@CarlisleWorkspace")

require(Rdistance)

# Load the example sparrows dataset
data(sparrows)







# # Add NA rows to counts where no sparrows were observed
# length(unique(sparrows.covs$TranID))  # unique IDs for all transects surveyed (even if no sparrow recorded)
# (absences <- data.frame(table(sparrows.counts$TranID)))  # Note sparrows not observed at all transects
# (absences <- as.character(absences[absences$Freq==0, 1]))
# 
# (toadd <- data.frame(matrix(nrow=length(absences), ncol=ncol(sparrows.counts))))
# toadd[, 1] <- absences
# names(toadd) <- names(sparrows.counts)
# 
# # Append NA rows
# sparrows.counts <- rbind(sparrows.counts, toadd)





# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
sparrows.counts$PerpDist <- perp.dists(obs.dist=sparrows.counts$SightDist, obs.angle=sparrows.counts$SightAngle, digits=1)

# Remove sight distance and angle
sparrows.counts <- sparrows.counts[, -c(3, 4)]

# Save vector of distances (includes NAs)
x <- sparrows.counts$PerpDist

# Examine the histogram of distances
hist(x)
rug(x)
summary(x)




# Fit detection function
?F.dfunc.estim
# (dfunc <- F.dfunc.estim(x, likelihood="uniform", w.hi=150))
(dfunc <- F.dfunc.estim(x, likelihood="halfnorm", w.hi=150))
# (dfunc <- F.dfunc.estim(x, likelihood="hazrate", w.hi=150))

# 72 transects surveyed, each 500 m
# area=10000 converts to density per ha (for dist measured in m)
?F.abund.estim
#(fit <- F.abund.estim(dfunc, group.sizes=sparrows.counts$Number, tot.trans.len=(72*500), area=10000, plot.bs=TRUE))
(fit <- F.abund.estim(dfunc, tot.trans.len=(72*500), area=10000, R=200, plot.bs=TRUE))
F.abund.estim(dfunc)
print(fit)
str(fit)

# How to index abundance estimate?
fit
# What are the names?
fit$ci

# plot
fit















## Next sections to tackle



?F.abund.estim
?F.gx.estim

# Throws error prior to bootstrapping:
#Error in key * (1 + (exp.term %*% a[2:(nexp + 1)])) : 
#    non-conformable arrays
F.automated.CDA(x, area=10000, total.trans.len=500, w.hi = 150,
                likelihoods=c("uniform", "halfnorm", "hazrate", "negexp", "Gamma"),
                series=c("simple", "cosine"), expansions=c(0, 1, 2, 3), plot=TRUE)

# uniform, 0 expansions selected as best
# density (per ha) is 0.96 (CI=0.84, 1.19)
F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi = 150,
                likelihoods=c("uniform", "halfnorm", "Gamma"),
                series=c("simple", "cosine"), expansions=c(0, 1, 2), plot=TRUE)

F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi=150,
                likelihoods=c("halfnorm", "uniform", "hazrate", "negexp", "Gamma"),
                series=c("simple", "cosine"), plot=TRUE)











## Junk code
# # Set working directory
# setwd("C:\\Users\\jcarlis3\\Box Sync\\Classes\\s8_Spr_15\\DistanceSampling\\ExampleVignette")


# # Read in example datasets
# counts <- read.csv("Sparrows.Counts.csv")
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

