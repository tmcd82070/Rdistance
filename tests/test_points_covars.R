#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the point-transect workflow, with covariates

# Jason D. Carlisle
# WEST, Inc.
# jcarlisle@west-inc.com
# Last updated 9/6/2017

# This demo was tested using the following:
# Rdistance version 2.0.0

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 2) Read in input data -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance requires two input datasets.  These can be prepared outside of R and read in as data.frames using,
# for example, read.csv.  We make use of the thrasher example datasets already contained within Rdistance.


# The first required dataset is a detection data.frame
# Each row is a detection, and the siteID, groupsize, and dist columns are required (as named)
data(thrasher.detections)
head(thrasher.detections)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(thrasher.sites)
head(thrasher.sites)



# CV of each predictor
apply(thrasher.sites[3:6], 2, function(x) sd(x)/mean(x))


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3) Fit a detection function
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3: Fit a detection function
# After prepping the input data, the first step is to explore your data and fit a detection function.


# Distance-sampling analysis is done on perpendicular distances (i.e., the distance from each detected group to the
# transect, not to the observer) for line transects and radial distances for point transects.  The radial distances are
# provided in thrasher.detections






# Explore the distribution of distances.
hist(thrasher.detections$dist, col="grey", main="", xlab="Distance (m)")
rug(thrasher.detections$dist)
summary(thrasher.detections$dist)


quantile(thrasher.detections$dist, c(0.5, 0.85, 0.9, 0.95))
trunc <- 175

# Next, fit a detection function (plotted as a red line) using `F.dfunc.estim`.  For now, we will proceed using the
# half-normal likelihood as the detection function, but in Section 5 of this tutorial, we demonstrate how to run an
# automated process that fits multiple detection functions and compares them using AICc.  Larger distances are quite
# sparse, so here we right-truncate the data, tossing out detections where `dist` > trunc.

# Merge site-level covariates to detection data
thrasher.merge <- merge(thrasher.detections, thrasher.sites, by="siteID")


thrasher.dfunc <- F.dfunc.estim(formula=dist~height, data=thrasher.merge, likelihood="halfnorm", w.hi=trunc, point.transects=TRUE)
plot(thrasher.dfunc)
thrasher.dfunc



mean(thrasher.merge$height)
mean(thrasher.sites$height)
plot(thrasher.dfunc)
thrasher.dfunc

# Plot for different covar values
plot(thrasher.dfunc, newdata=data.frame(height=seq(min(thrasher.sites$height), max(thrasher.sites$height), length.out=4)))

# And add more bins
plot(thrasher.dfunc, newdata=data.frame(height=seq(min(thrasher.sites$height), max(thrasher.sites$height), length.out=4)),
     nbins=20)




ESW(thrasher.dfunc)
effective.radius(thrasher.dfunc)

# The effective strip width (ESW) is the key information from the detection function that will be used to next estimate
# abundance (or density).  The ESW is calculated by integrating under the detection function.  A survey with imperfect
# detection and ESW equal to *X* effectively covers the same area as a study with perfect detection out to a distance
# of *X*.  See the help documentation for `ESW` for details.
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 4) Estimate abundance given the detection function -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Estimating abundance requires the additional information contained in the second required dataset, described earlier,
# where each row represents one transect. Load the example dataset of surveyed thrasher transects from the package.


# Next, estimate abundance (or density in this case) using `F.abund.estim`.  If `area`=1, then density is given in the
# squared units of the distance measurements --- in this case, thrashers per square meter.  Instead, we set `area`=10000
# in order to convert to thrashers per hectare (1 ha == 10,000 m^2^).  The equation used to calculate the abundance
# estimate is detailed in the help documentation for `F.abund.estim`.

# Confidence intervals for abundance are calculated using a bias-corrected bootstrapping method (see `F.abund.estim`),
# and the detection function fit in each iteration of the bootstrap is plotted as a blue line (if `plot.bs=TRUE`).
# Note that, as with all bootstrapping procedures, there may be slight differences in the confidence intervals between
# runs due to so-called 'simulation slop'.  Increasing the number of bootstrap iterations (`R` = 100 used here) may be
# necessary to stabilize CI estimates.

# (jdc) the plotting of the bootstrap isn't right...
fit <- F.abund.estim(dfunc=thrasher.dfunc, detection.data=thrasher.detections, site.data=thrasher.sites,
                     area=10000, R=20, ci=0.95, plot.bs=TRUE)
fit



# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci


# 
# fitby <- F.abund.estim(dfunc=thrasher.dfunc, detection.data=thrasher.detections, site.data=thrasher.sites,
#                      area=10000, R=100, ci=0.95, plot.bs=TRUE, by.id=TRUE)
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


# auto <- F.automated.CDA(formula=dist~1, detection.data=thrasher.detections, site.data=thrasher.sites,
#                         w.hi=trunc, plot=FALSE, area=10000, R=100, ci=0.95, plot.bs=TRUE, point.transects=TRUE)
# 



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#




# Compare to Distance

# I'VE TRIED WITH EITHER SHRUB OR BARE AS COVARIATES, AND THE DETECTION PROB IS FLAT FOR BOTH...
# THE MODEL DOES WORK FOR HEIGHT, BUT I SUSPECT THE ISSUE IS THAT THERE'S LITTLE VARIATION IN THE COVARIATE
# VALUES ACROSS SITES.


# Format detection data
d.data <- thrasher.merge[c("dist", "groupsize", "height")]
names(d.data) <- c("distance", "size", "height")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- data.frame(Sample.Label = thrasher.sites$siteID, Effort = 1)  # Effort for points is number of visits (see ?flatfile)
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=thrasher.detections$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, formula= ~height, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="point", key="hn", adjustment=NULL, dht.group=FALSE))

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


