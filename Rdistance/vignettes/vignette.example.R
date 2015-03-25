# Vignette for the Rdistance Package
# Distance-sampling analysis in Rdistance

# Prepared by:
# Jason D. Carlisle
# Wyoming Cooperative Fish & Wildlife Research Unit, University of Wyoming
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com
# Assistance from Trent L. McDonald, WEST, Inc.

# Last updated 3/25/2015

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
# Automate fitting multiple detection functions and estimate abund --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


F.automated.CDA(distdata=dists.df, covdata=covs.df, area=10000, R=500, ci=0.95, plot.bs=TRUE,
                bs.method="transects", w.hi=150)








# Compare to fitting the best model outside of automation
(dfunc <- F.dfunc.estim(dists.df, likelihood="negexp", expansions=1, series="cosine", w.hi=150))
F.abund.estim(dfunc, distdata=dists.df, covdata=covs.df, area=10000, R=500, ci=0.95, plot.bs=TRUE, bs.method="transects")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Example with two groups -------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data(sparrows)

# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
dists.df$dists <- perp.dists(obs.dist=dists.df$sightdist, obs.angle=dists.df$sightangle, digits=1)

# Remove sight distance and angle
dists.df <- dists.df[, -which(names(dists.df) %in% c("sightdist", "sightangle"))]


# split the covariats and distance dataframe by sagebrush cover class
hi.covs <- covs.df[covs.df$sage=="High", ]
lo.covs <- covs.df[covs.df$sage=="Low", ]

hi.dists <- dists.df[dists.df$siteID %in% hi.covs$siteID, ]
lo.dists <- dists.df[dists.df$siteID %in% lo.covs$siteID, ]

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
                           plot.bs=TRUE, bs.method="transects", w.hi=150))

(lo.fit <- F.automated.CDA(distdata=lo.dists, covdata=lo.covs, area=10000, R=200, ci=0.95,
                           plot.bs=TRUE, bs.method="transects", w.hi=150))




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





