#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the point-transect workflow, with covariates

# Jason D. Carlisle
# WEST, Inc.
# jcarlisle@west-inc.com
# Last updated 9/21/2017

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
# context("Test the lines_points_Covars() function")

test_that("lines_points_Covars() operates as it should", {
data(thrasherDetectionData)
head(thrasherDetectionData)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(thrasherSiteData)
head(thrasherSiteData)







# CV of each predictor
apply(thrasherSiteData[3:6], 2, function(x) sd(x)/mean(x))



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3) Fit a detection function
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 3: Fit a detection function
# After prepping the input data, the first step is to explore your data and fit a detection function.


# Distance-sampling analysis is done on perpendicular distances (i.e., the distance from each detected group to the
# transect, not to the observer) for line transects and radial distances for point transects.  The radial distances are
# provided in thrasherDetectionData






# Explore the distribution of distances.
hist(thrasherDetectionData$dist, col="grey", 
     main="", xlab="Distance (m)", breaks=20)
rug(thrasherDetectionData$dist)
summary(thrasherDetectionData$dist)


quantile(thrasherDetectionData$dist, c(0.5, 0.85, 0.9, 0.95))
trunc <- 175

# Next, fit a detection function (plotted as a red line) using `dfuncEstim`.  For now, we will proceed using the
# half-normal likelihood as the detection function, but in Section 5 of this tutorial, we demonstrate how to run an
# automated process that fits multiple detection functions and compares them using AICc.  Larger distances are quite
# sparse, so here we right-truncate the data, tossing out detections where `dist` > trunc.




thrasher.dfunc <- dfuncEstim(formula=dist~observer, 
                             detectionData=thrasherDetectionData,
                             siteData=thrasherSiteData, 
                             likelihood="halfnorm", 
                             w.hi=trunc, 
                             pointSurvey=TRUE)
thrasher.dfunc


# Plot for different covar values

(newdata <- data.frame(observer=factor(c("obs1","obs2"),levels=paste0("obs",1:6))))
plot(thrasher.dfunc, newdata=newdata)





# EDR(thrasher.dfunc)
effectiveDistance(thrasher.dfunc)
mean(effectiveDistance(thrasher.dfunc))

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 4) Estimate abundance given the detection function -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Estimating abundance requires the additional information contained in the second required dataset, described earlier,
# where each row represents one transect. Load the example dataset of surveyed thrasher transects from the package.


# Next, estimate abundance (or density in this case) using `abundEstim`.  If `area`=1, then density is given in the
# squared units of the distance measurements --- in this case, thrashers per square meter.  Instead, we set `area`=10000
# in order to convert to thrashers per hectare (1 ha == 10,000 m^2^).  The equation used to calculate the abundance
# estimate is detailed in the help documentation for `abundEstim`.

# Confidence intervals for abundance are calculated using a bias-corrected bootstrapping method (see `abundEstim`),
# and the detection function fit in each iteration of the bootstrap is plotted as a blue line (if `plot.bs=TRUE`).
# Note that, as with all bootstrapping procedures, there may be slight differences in the confidence intervals between
# runs due to so-called 'simulation slop'.  Increasing the number of bootstrap iterations (`R` = 100 used here) may be
# necessary to stabilize CI estimates.

# (jdc) the plotting of the bootstrap isn't right...
fit <- abundEstim(dfunc=thrasher.dfunc, detectionData=thrasherDetectionData, siteData=thrasherSiteData,
                     area=10000, R=5, ci=0.95, plot.bs=TRUE)
fit



# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci



fitBy <- abundEstim(dfunc=thrasher.dfunc, detectionData=thrasherDetectionData, siteData=thrasherSiteData,
                     area=10000, ci=NULL, bySite=TRUE)
head(fitBy)

mean(fitBy$density)
fit$n.hat




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


auto <- autoDistSamp(formula=dist~height, detectionData=thrasherDetectionData, siteData=thrasherSiteData,
                     w.hi=trunc, plot=FALSE, area=10000, R=5, ci=0.95, plot.bs=TRUE, pointSurvey=TRUE,
                     likelihoods=c("halfnorm",  "uniform"))






#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#




# Compare to Distance
require(Distance)

# I'VE TRIED WITH EITHER SHRUB OR BARE AS COVARIATES, AND THE DETECTION PROB IS FLAT FOR BOTH...
# THE MODEL DOES WORK FOR HEIGHT, BUT I SUSPECT THE ISSUE IS THAT THERE'S LITTLE VARIATION IN THE COVARIATE
# VALUES ACROSS SITES.

 

# Format detection data
thrasher.merge <- merge(thrasherDetectionData, thrasherSiteData, by="siteID")
d.data <- thrasher.merge[c("dist", "groupsize", "dummy")]
names(d.data) <- c("distance", "size", "dummy")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- data.frame(Sample.Label = thrasherSiteData$siteID, Effort = 1)  # Effort for points is number of visits (see ?flatfile)
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=thrasherDetectionData$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, formula= ~dummy, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="point", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)  


# With dummy as covariate
# Results similar, but don't match exactly
# AIC
AIC(fit)  # Rdistance = 1754.642
AIC(ds.fit)  # Distance = 1754.579

# Abund
fit$n.hat  # 0.491037
fit$ci  # depends on bootstrap, but around 0.39 - 0.58
ds.fit$dht$individuals$N  # 0.4910311 (0.3917012 - 0.6155497)
# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

# Sigma
coef(fit)  # Intercept = 4.63149158, dummy = -0.09736296
ds.fit$ddf$ds$aux$ddfobj$scale$parameters  # Intercept = 4.63150603, dummy = -0.09736732
})


