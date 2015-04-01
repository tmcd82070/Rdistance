# Vignette for the Rdistance Package
# Distance-sampling analysis in Rdistance using the sparrow dataset

# Prepared by:
# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit, University of Wyoming
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com
# Assistance from Trent L. McDonald, WEST, Inc.

# Last updated 3/30/2015

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Install package ---------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## USE CURRENT GITHUB VERSION OF THE PACKAGE
# Install devtools in order to install development version of Rdistance from GitHub
require(devtools)

# Install the development version of Rdistance from the CarlisleWorkspace branch on GitHub
install_github("tmcd82070/Rdistance/Rdistance@CarlisleWorkspace")
require(Rdistance)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Import and prep data ----------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Load the example sparrows distances dataset from package
data(sparrow.dists)

# Compute perpendicular, off-transect distances from the observer's sight distance and angle
sparrow.dists$dists <- perp.dists(obs.dist=sparrow.dists$sightdist, obs.angle=sparrow.dists$sightangle)

# Remove sight distance and angle
sparrow.dists <- sparrow.dists[, -which(names(sparrow.dists) %in% c("sightdist", "sightangle"))]

# Save vector of distances
x <- sparrow.dists$dists

# Examine the histogram of distances
hist(x)
rug(x)
summary(x)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit a detection function ------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Fit detection function
# Requires only a vector of distances
(dfunc <- F.dfunc.estim(x, likelihood="halfnorm", w.hi=150))  # supplying just the vector of distances

# same as supplying the data.frame with "dists" column
F.dfunc.estim(sparrow.dists, likelihood="halfnorm", w.hi=150)



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
data(sparrow.covs)


# 72 transects surveyed, each 500 m
# area=10000 converts to density per ha (for dist measured in m)

(fit <- F.abund.estim(dfunc, distdata=sparrow.dists, covdata=sparrow.covs, area=10000,
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
# Automate fitting multiple detection functions and estimate abund --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


F.automated.CDA(distdata=sparrow.dists, covdata=sparrow.covs, area=10000, R=500, ci=0.95, plot.bs=TRUE,
                bs.method="transects", w.hi=150)








# Compare to fitting the best model outside of automation
(dfunc <- F.dfunc.estim(sparrow.dists, likelihood="negexp", expansions=1, series="cosine", w.hi=150))
F.abund.estim(dfunc, distdata=sparrow.dists, covdata=sparrow.covs, area=10000, R=500, ci=0.95, plot.bs=TRUE, bs.method="transects")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Example with two groups -------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# split the covariats and distance dataframe by sagebrush cover class
hi.covs <- sparrow.covs[sparrow.covs$sage=="High", ]
lo.covs <- sparrow.covs[sparrow.covs$sage=="Low", ]

hi.dists <- sparrow.dists[sparrow.dists$siteID %in% hi.covs$siteID, ]
lo.dists <- sparrow.dists[sparrow.dists$siteID %in% lo.covs$siteID, ]

# histograms
hist(hi.dists$dists)
hist(lo.dists$dists)


# fit dfunc objects
(hi.dfunc <- F.dfunc.estim(hi.dists, likelihood="halfnorm", w.hi=150))
(lo.dfunc <- F.dfunc.estim(lo.dists, likelihood="halfnorm", w.hi=150))


# estimate abundance
(hi.fit <- F.abund.estim(hi.dfunc, distdata=hi.dists, covdata=hi.covs, area=10000,
                         R=500, ci=0.95, plot.bs=TRUE, bs.method="transects"))

(lo.fit <- F.abund.estim(lo.dfunc, distdata=lo.dists, covdata=lo.covs, area=10000,
                         R=500, ci=0.95, plot.bs=TRUE, bs.method="transects"))



# compile results for plotting
pdata <- data.frame(sage=c("Low", "High"))
pdata$sage <- factor(pdata$sage, levels=c("Low", "High"))
pdata$nhat <- c(lo.fit$n.hat, hi.fit$n.hat)
pdata$low <- c(lo.fit$ci[1], hi.fit$ci[1])
pdata$upp <- c(lo.fit$ci[2], hi.fit$ci[2])

# plot using ggplot2 package
require(ggplot2)
ggplot(data=pdata, aes(x=sage, y=nhat, fill=sage)) +
  geom_bar(stat="identity", colour="black") +
  geom_errorbar(aes(ymin=low, ymax=upp), width=.1) +
  xlab("Sagebrush Cover") + ylab("Sparrow Density\n(birds per ha)") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() + theme(legend.position="none")




##
# automated option
(hi.fit <- F.automated.CDA(distdata=hi.dists, covdata=hi.covs, area=10000, R=200, ci=0.95,
                           plot.bs=TRUE, bs.method="transects", w.hi=150, plot=FALSE))

(lo.fit <- F.automated.CDA(distdata=lo.dists, covdata=lo.covs, area=10000, R=200, ci=0.95,
                           plot.bs=TRUE, bs.method="transects", w.hi=150, plot=FALSE))




# compile results for plotting
pdata <- data.frame(sage=c("Low", "High"))
pdata$sage <- factor(pdata$sage, levels=c("Low", "High"))
pdata$nhat <- c(lo.fit$n.hat, hi.fit$n.hat)
pdata$low <- c(lo.fit$ci[1], hi.fit$ci[1])
pdata$upp <- c(lo.fit$ci[2], hi.fit$ci[2])

# plot using ggplot2 package
require(ggplot2)
ggplot(data=pdata, aes(x=sage, y=nhat, fill=sage)) +
  geom_bar(stat="identity", colour="black") +
  geom_errorbar(aes(ymin=low, ymax=upp), width=.1) +
  xlab("Sagebrush Cover") + ylab("Sparrow Density\n(birds per ha)") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_bw() + theme(legend.position="none")







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Junk code ---------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




