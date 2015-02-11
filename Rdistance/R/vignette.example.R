# Vignette for the Rdistance Package
# An example distance-sampling analysis workflow using the Rdistance package

# Prepared by:
# Jason Carlisle
# WY Cooperative Fish & Wildlife Research Unit
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com

# Assistance from Trent McDonald, WEST, Inc.

# Last updated 2/2/2015



# Source current functions from CarlisleWorkspace branch
fundir <- "C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/Rdistance/Rdistance/R"
source(paste(fundir, "perp.dists.R", sep="/"))
source(paste(fundir, "F.dfunc.estim.R", sep="/"))


# Load or Install/load the Rdistance package (currently version 1.1)
if(!require(Rdistance)) { 
    install.packages("Rdistance")
    require(Rdistance)
}


# Load example dataset
load("C:/Users/jcarlis3/Box Sync/Classes/s8_Spr_15/DistanceSampling/Rdistance/Rdistance/data/sparrows.rda")

# data(sparrows)


# Add NA rows to counts where no sparrows were observed
length(unique(sparrows.covs$TranID))  # unique IDs for all transects surveyed (even if no sparrow recorded)
(absences <- data.frame(table(sparrows.counts$TranID)))  # Note sparrows not observed at all transects
(absences <- as.character(absences[absences$Freq==0, 1]))

(toadd <- data.frame(matrix(nrow=length(absences), ncol=ncol(sparrows.counts))))
toadd[, 1] <- absences
names(toadd) <- names(sparrows.counts)

# Append NA rows
sparrows.counts <- rbind(sparrows.counts, toadd)





# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
sparrows.counts$PerpDist <- perp.dists(obs.dist=sparrows.counts$SightDist, obs.angle=sparrows.counts$SightAngle, digits=1)

# Save vector of distances (includes NAs)
x <- sparrows.counts$PerpDist

# Examine the histogram of distances
hist(x)
summary(x)




# Fit detection function
?F.dfunc.estim
(dfunc <- F.dfunc.estim(x, likelihood="uniform", w.hi=150))
# (dfunc <- F.dfunc.estim(x, likelihood="halfnorm", w.hi=150))
# (dfunc <- F.dfunc.estim(x, likelihood="hazrate", w.hi=150))


# 72 transects surveyed, each 500 m
# area=10000 converts to density per ha (for dist measured in m)
?F.abund.estim
#(fit <- F.abund.estim(dfunc, group.sizes=sparrows.counts$Number, tot.trans.len=(72*500), area=10000, plot.bs=TRUE))
(fit <- F.abund.estim(dfunc, tot.trans.len=(72*500), area=10000, R=200, plot.bs=TRUE))
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
