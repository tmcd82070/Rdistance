#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance:  an R package for distance-sampling analysis

# This script tests the line-transect workflow, with no covariates

# Jason D. Carlisle
# WEST, Inc.
# jcarlisle@west-inc.com
# Last updated 9/5/2017

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
# ?sparrowDetectionData
# ?sparrowSiteData
# #////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# 2) Read in input data -----
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#
# Rdistance requires two input datasets.  These can be prepared outside of R and read in as data.frames using,
# for example, read.csv.  We make use of the sparrow example datasets already contained within Rdistance.


# The first required dataset is a detection data.frame
# Each row is a detection, and the siteID, groupsize, and dist columns are required (as named)
data(sparrowDetectionData)
head(sparrowDetectionData)



# If the observers recorded sighting distance and sighting angle instead of perpendicular distance (as is often common
# in line transect surveys), you can use the `perp.dists` function (detailed in Section 3) to calculate the perpendicular
# distances based on the sighting distances and sighting angles.



# The second required dataset is a transect data.frame
# Each row is a transect, and the siteID and length columns are required (as named)
# Other columns (e.g., transect-level covariates) are ignored, but may be useful in modeling abundance later
data(sparrowSiteData)
head(sparrowSiteData)
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


# sparrowDetectionData$dist2 <- perp.dists(s.dist="sightdist", s.angle="sightangle", data=sparrowDetectionData)
# 
# sparrowDetectionData <- sparrowDetectionData[, -which(names(sparrowDetectionData) %in% c("sightdist", "sightangle", "dist2"))]                                                                  
# head(sparrowDetectionData)



# Explore the distribution of distances.
hist(sparrowDetectionData$dist, col="grey", main="", xlab="Distance (m)")
rug(sparrowDetectionData$dist)
summary(sparrowDetectionData$dist)





# Next, fit a detection function (plotted as a red line) using `F.dfunc.estim`.  For now, we will proceed using the
# half-normal likelihood as the detection function, but in Section 5 of this tutorial, we demonstrate how to run an
# automated process that fits multiple detection functions and compares them using AICc.  Note that distances greater
# than 150 m are quite sparse, so here we right-truncate the data, tossing out detections where `dist` > 150.
trunc <- 150
sparrow.dfunc <- F.dfunc.estim(formula=dist~1, detection.data=sparrowDetectionData, likelihood="halfnorm", w.hi=trunc)
plot(sparrow.dfunc)
sparrow.dfunc


ESW(sparrow.dfunc)
effectiveDistance(sparrow.dfunc)

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

fit <- F.abund.estim(dfunc=sparrow.dfunc, detection.data=sparrowDetectionData, site.data=sparrowSiteData,
                     area=10000, R=100, ci=0.95, plot.bs=TRUE)
fit






# Results of interest (such as the abundance estimate and confidence interval) can be extracted from the resulting
# object (here called `fit`).
fit$n.hat
fit$ci

# 
# fitby <- F.abund.estim(dfunc=sparrow.dfunc, detection.data=sparrowDetectionData, site.data=sparrowSiteData,
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


# auto <- F.automated.CDA(formula=dist~1, detection.data=sparrowDetectionData, site.data=sparrowSiteData,
#                         w.hi=trunc, plot=FALSE, area=10000, R=100, ci=0.95, plot.bs=TRUE)


# Before the package overhaul, the top-ranked detection function was the negative
# exponential likelihood, with one cosine expansion.

# Now the uniform, 0 expansions is top-ranked.

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#

# This used to be the best-fitting detection function
# best.old <- F.dfunc.estim(formula=dist~1, data=sparrowDetectionData, likelihood="negexp", expansions=1, series="cosine", w.hi=150)
# plot(best.old)
# best.old




# Compare to Distance

# Format detection data
d.data <- sparrowDetectionData[c("dist", "groupsize")]
names(d.data) <- c("distance", "size")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- sparrowSiteData[c("siteID", "length")]
names(d.sample)[1:2] <- c("Sample.Label", "Effort")
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=sparrowDetectionData$siteID)

# Fit model and estimate abundance
(ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="line", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)



# ESW, p, and nhat all match
