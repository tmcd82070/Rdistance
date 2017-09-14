#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the line-transect workflow, with covariates

# Jason D. Carlisle
# WEST, Inc.
# jcarlisle@west-inc.com
# Last updated 9/6/2017

# This demo was tested using the following:
# Rdistance version 2.0.0

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Outline -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# This script contains the following sections:
# 1) Install and load Rdistance
# 2) Read in input data
# 3) Fit a detection function
# 4) Estimate abundance given the detection function
# 5) Use AICc to select a detection function and estimate abundance
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# # 1) Install and load Rdistance -----
# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# # As of publication, the Rdistance package is not on CRAN, but can be installed from a GitHub repository, which will
# # require using the devtools package.  The Rdistance package may be further developed after publication, but this demo
# # is current and compatible with Rdistance version 1.3.2.
# 
# # Check if Rdistance is installed.  If yes, load it.  If not, install it, then load it.
# # Rdistance depends only on base R packages
# 
# if("Rdistance" %in% rownames(installed.packages()) == FALSE){
#   install.packages("Rdistance")
# }
# require(Rdistance)
# 
# 
# # View help documentation for the Rdistance package
# ?Rdistance
# 
# # View help documentation for key Rdistance functions
# ?F.dfunc.estim
# ?F.abund.estim
# ?F.automated.CDA
# 
# # View help documentation for Rdistance example datasets
# ?sparrow.detections
# ?sparrow.sites
# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 2) Read in input data -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance requires two input datasets.  These can be prepared outside of R and read in as data.frames using,
# for example, read.csv.  We make use of the sparrow example datasets already contained within Rdistance.


# The first required dataset is a detection data.frame
# Each row is a detection, and the siteID, groupsize, and dist columns are required (as named)
data(sparrow.detections)
head(sparrow.detections)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(sparrow.sites)
head(sparrow.sites)
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3) Fit a detection function
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3: Fit a detection function
# After prepping the input data, the first step is to explore your data and fit a detection function.


# Distance-sampling analysis is done on perpendicular distances (i.e., the distance from each detected group to the
# transect, not to the observer).  We have provided the perpendicular distances (named `dist`) in the example data, but
# observers originally recorded sighting distances and sighting angles.  Here we use the `perp.dists` function to
# (re)calculate the perpendicular distances (`dist`) and remove the `sightdist` and `sightangle` columns.  See the help
# documentation for `perp.dists` for details.


# sparrow.detections$dist2 <- perp.dists(s.dist="sightdist", s.angle="sightangle", data=sparrow.detections)
# 
# sparrow.detections <- sparrow.detections[, -which(names(sparrow.detections) %in% c("sightdist", "sightangle", "dist2"))]                                                                  
# head(sparrow.detections)



# Explore the distribution of distances.
hist(sparrow.detections$dist, col="grey", main="", xlab="Distance (m)", breaks=20)
rug(sparrow.detections$dist)
summary(sparrow.detections$dist)





# Next, fit a detection function (plotted as a red line) using `F.dfunc.estim`.  For now, we will proceed using the
# half-normal likelihood as the detection function, but in Section 5 of this tutorial, we demonstrate how to run an
# automated process that fits multiple detection functions and compares them using AICc.  Note that distances greater
# than 150 m are quite sparse, so here we right-truncate the data, tossing out detections where `dist` > 150.

# Merge site-level covariates to detection data
sparrow.merge <- merge(sparrow.detections, sparrow.sites, by="siteID")

trunc <- 150
sparrow.dfunc <- F.dfunc.estim(formula=dist~shrub, data=sparrow.merge, likelihood="halfnorm", w.hi=trunc)
plot(sparrow.dfunc)
sparrow.dfunc

# Convergence failure when covariate is bare
# x <- F.dfunc.estim(formula=dist~bare, data=sparrow.merge, likelihood="halfnorm", w.hi=trunc)
# Warning message:
#   In F.dfunc.estim(formula = dist ~ bare, data = sparrow.merge, likelihood = "halfnorm",  :
#                      ERROR: ABNORMAL_TERMINATION_IN_LNSRCH



# sparrow.dfunc <- F.dfunc.estim(formula=dist~shrub, data=sparrow.merge, likelihood="halfnorm", w.hi=150)
mean(sparrow.merge$shrub)
mean(sparrow.sites$shrub)
plot(sparrow.dfunc)
sparrow.dfunc

# Plot for different covar values
plot(sparrow.dfunc, newdata=data.frame(shrub=seq(min(sparrow.sites$shrub), max(sparrow.sites$shrub), length.out=4)))

# And add more bins
plot(sparrow.dfunc, newdata=data.frame(shrub=seq(min(sparrow.sites$shrub), max(sparrow.sites$shrub), length.out=4)),
     nbins=20)


# # abund
# fit <- F.abund.estim(sparrow.dfunc, detection.data=sparrow.detections, transect.data=sparrow.sites,
#                      area=10000, R=100, ci=0.95, plot.bs=TRUE)
# 
# fit










# The effective strip width (ESW) is the key information from the detection function that will be used to next estimate
# abundance (or density).  The ESW is calculated by integrating under the detection function.  A survey with imperfect
# detection and ESW equal to *X* effectively covers the same area as a study with perfect detection out to a distance
# of *X*.  See the help documentation for `ESW` for details.
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 4) Estimate abundance given the detection function -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Estimating abundance requires the additional information contained in the second required dataset, described earlier,
# where each row represents one transect. Load the example dataset of surveyed sparrow transects from the package.


# Next, estimate abundance (or density in this case) using `F.abund.estim`.  If `area`=1, then density is given in the
# squared units of the distance measurements --- in this case, sparrows per square meter.  Instead, we set `area`=10000
# in order to convert to sparrows per hectare (1 ha == 10,000 m^2^).  The equation used to calculate the abundance
# estimate is detailed in the help documentation for `F.abund.estim`.

# Confidence intervals for abundance are calculated using a bias-corrected bootstrapping method (see `F.abund.estim`),
# and the detection function fit in each iteration of the bootstrap is plotted as a blue line (if `plot.bs=TRUE`).
# Note that, as with all bootstrapping procedures, there may be slight differences in the confidence intervals between
# runs due to so-called 'simulation slop'.  Increasing the number of bootstrap iterations (`R` = 100 used here) may be
# necessary to stabilize CI estimates.


# (jdc) the bootstrap-replicate detection lines (plot.bs=TRUE) aren't in the right place, but the bootstrap-generated CIs appear plausible
# 
fit <- F.abund.estim(dfunc=sparrow.dfunc, detection.data=sparrow.detections, site.data=sparrow.sites,
                     area=10000, R=75, ci=0.95, plot.bs=TRUE)
fit






# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci

# 
# fitby <- F.abund.estim(dfunc=sparrow.dfunc, detection.data=sparrow.detections, site.data=sparrow.sites,
#                        area=10000, R=100, ci=0.95, plot.bs=TRUE, by.id=TRUE)
# fitby


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 5) Use AICc to select a detection function and estimate abundance -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Alternatively, steps 3 (fitting a detection function) and 4 (estimating abundance) can be automated using the
# function `F.automated.CDA`.  This function attempts to fit multiple detection functions, uses AICc (by default, but
# see help documentation for `AIC.dfunc` for other options) to find the 'best' detection function, then proceeds to
# estimate abundance using that detection function.  By default, `F.automated.CDA` tries a large subset of `Rdistance`'s
# built-in detection functions, but you can control exactly which detection functions are attempted (see help
# documentation for `F.automated.CDA`).  Specifying `plot=TRUE` would return a plot of each detection function.  In
# this example, we attempt to fit the default detection functions (n = 41), and we don't plot each (`plot=FALSE`).


# auto <- F.automated.CDA(formula=dist~1, detection.data=sparrow.detections, site.data=sparrow.sites,
#                         w.hi=trunc, plot=FALSE, area=10000, R=100, ci=0.95, plot.bs=TRUE)


# Before the package overhaul, the top-ranked detection function was the negative
# exponential likelihood, with one cosine expansion.

# Now the uniform, 0 expansions is top-ranked.

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

# This used to be the best-fitting detection function
# best.old <- F.dfunc.estim(formula=dist~1, data=sparrow.detections, likelihood="negexp", expansions=1, series="cosine", w.hi=150)
# plot(best.old)
# best.old




# Compare to Distance

# Format detection data
d.data <- sparrow.merge[c("dist", "groupsize", "shrub")]
names(d.data) <- c("distance", "size", "shrub")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- sparrow.sites[c("siteID", "length")]
names(d.sample)[1:2] <- c("Sample.Label", "Effort")
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrow.detections$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, formula= ~shrub, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)



# Results similar, but don't match exactly
# AIC
AIC(fit)  # Rdistance
AIC(ds.fit)  # Distance

# Abund
fit$n.hat
fit$ci
ds.fit$dht$individuals$N
# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

# Sigma
coef(fit)
ds.fit$ddf$ds$aux$ddfobj$scale$parameters


